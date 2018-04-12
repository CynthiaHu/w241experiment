# test and prep for gps data, for use in geolocation clustering
# to be merged into main test script


getwd()
setwd("C:/Users/yangyq/workspaces/ucbiyyq/w241experiment/analysis")
getwd()


library(data.table)

# loads raw data
dt <- data.table(read.csv("../data/Data - BOTH.csv", na.strings=c("")))

# creates some dummy variables for ease of query and analysis later
dtA <- dt[
    ,.(
        Number=factor(Number)
        ,Street=factor(Street)
        ,Zip=factor(Zip)
        ,Route
        ,PickupDOW
        ,Pre
        ,Treat
        ,Post
        ,S=factor(ifelse(Pre=="N",1,0))
        ,D=ifelse(Treat=="Y",1,0)
        ,pre.bin=ifelse(Pre=="Y",1,0)
        ,post.bin=ifelse(Post=="Y",1,ifelse(Post=="N",0,NA))
        ,wed=ifelse(PickupDOW=="WED",1,0)
    )
    ,
    ] 


# calculates the street-score covariate
# we might expect that a person on a street where lots of households recycle, might be more inclined to recycle
# street score is calculated as the ratio of: number of Pre==Y, to number of houses on the street, for each street

# calculates the street score data table
s.scr <- merge( 
    x=dtA[Pre=="Y",.(n.pre.Y=.N),by="Street"]
    ,y=dtA[,.(n.houses=.N),by="Street"]
    ,by.x="Street"
    ,by.y="Street"
    ,all.y=TRUE
)[
    ,.(
        Street
        #,count.Y
        #,count
        ,street.score=n.pre.Y/n.houses
    )
    ,
    ]


# joins the street score data table back into the main data table
dtA <- merge( x=dtA, y=s.scr, by.x="Street", by.y="Street" )










# test calls to Google geolocation API
# see https://developers.google.com/maps/documentation/geocoding/start

# example call with Google geolocation API call
# https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM



# see https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
library(httr)
#url <- "https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM"
#url <- "https://maps.googleapis.com/maps/api/geocode/json?address=10108+Chariot+Court+20850&key=AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM"
url <- "https://maps.googleapis.com/maps/api/geocode/json"
#url <- param_set(url, key = "address", value = "10108+Chariot+Court+20850")
url <- param_set(url, key = "address", value = paste("10108", "Chariot Court", "20850"))
url <- param_set(url, key = "key", value = "AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM")

r <- GET(url)



# see https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)
rc <- content(r,"text")
rjs <- fromJSON(txt=rc)

#toJSON(rjs$results, pretty=TRUE, digits=20)
toJSON(rjs$results$geometry$location, digits=20, pretty=TRUE)
toJSON(rjs$results$geometry$location, digits=20)
typeof(rjs$results$geometry$location) # list
typeof(rjs$results$geometry$location$lat) # double
format(round(rjs$results$geometry$location$lat, 10), nsmall = 10)
format(rjs$results$geometry$location$lat, nsmall = 10)
format(rjs$results$geometry$location$lng, nsmall = 10)



# see https://cran.r-project.org/web/packages/urltools/vignettes/urltools.html
#install.packages("urltools")
library(urltools)

ggeo <- function(num, str, zip) {
    u <- "https://maps.googleapis.com/maps/api/geocode/json"
    u <- param_set(u, key = "address", value = gsub(" ","+",paste(num, str, zip)))
    u <- param_set(u, key = "key", value = "AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM")
    r <- GET(u)
    rc <- content(r,"text")
    rjs <- fromJSON(rc)
    lat.lng <- rjs$results$geometry$location
    return(paste(lat.lng$lat, lat.lng$lng))
}

# calls the google geolocation API for every address in the data set
gps <- dtA[
    ,.(
        lat.lng=ggeo(Number,Street,Zip)
        )
    ,by=c("Number","Street","Zip")
][
    ,c("lat","lng"):=tstrsplit(lat.lng," ",fixed=TRUE)
    ,
][
    ,.(Number,Street,Zip,lat,lng)
]

# saves the gps results to disk
fwrite(gps,"../data/Data-gps.csv", quote="auto", sep=",", eol="\n", na="")




# reads gps from disk, if necessary
gpsr <- data.table(read.csv("../data/Data-gps.csv", na.strings=c("")))





# calculates the cross-join of each house to others
gpsj <- gpsr[,.(Number, Street, Zip, lat, lng, j=1),] #adds a dummy column "j" to manipulate the cross join
gpsx <- merge(gpsj, gpsj, by='j', allow.cartesian=TRUE)






#calculates the distance between gps points

# R function that computes the "great circle" distance between two gps points. Intended for use with data.table
# https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-distance-between-pair-of-coordinates-using-data-tab
# r = radius of earth in meters
# returns distance between the two gps locations, in meters
dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
    radians <- pi/180
    lat_to <- lat_to * radians
    lat_from <- lat_from * radians
    lon_to <- lon_to * radians
    lon_from <- lon_from * radians
    dLat <- (lat_to - lat_from)
    dLon <- (lon_to - lon_from)
    a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
    return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}

# calculates all the distances between all houses
gpsx <- gpsx[ 
    , .(
            Number.x
            ,Street.x
            ,Zip.x
            ,lat.x
            ,lng.x
            ,Number.y
            ,Street.y
            ,Zip.y
            ,lat.y
            ,lng.y
            ,dist=dt.haversine(lat.x, lng.x, lat.y, lng.y)
        )
    , 
]

# eyeball check of gps data for every house
summary(gpsr[,.(lat,lng)])

# distribution of the distances between houses
hist(gpsx$dist, breaks=1000)



# see https://www.rdocumentation.org/packages/ggforce/versions/0.1.1/topics/facet_wrap_paginate


for (i in unique(gpsr[,Street])) {
    #ggplot(data=gpsr[Street=="Potomac Oaks Drive",.(Street,lat,lng)], mapping=aes(x=lng,y=lat,color=Street)) +
    ggplot(data=gpsr[Street==i,.(Street,lat,lng)], mapping=aes(x=lng,y=lat,color=Street)) +
        geom_point(size=5) + 
        ggtitle(label="Binky") +
        xlab("Longitude") +
        ylab("Latitude")
}





# finds the closest neighbors
gpsx[dist < 50 & dist > 0, 
        .(Number.x
            ,Street.x
            ,Zip.x
            ,Number.y
            ,Street.y
            ,Zip.y
            ,dist)
     , ]

gpsx[dist < 20, 
     .(Number.x
       ,Street.x
       ,Zip.x
       ,Number.y
       ,Street.y
       ,Zip.y
       ,dist)
     , ]


# finds the fartherest neighbors
gpsx[dist >= 50, 
        .(Number.x
            ,Street.x
            ,Zip.x
            ,Number.y
            ,Street.y
            ,Zip.y
            ,dist)
     , ]

gpsx[dist < 50,.(Number.x,Lat.x,Long.x,Number.y,Lat.y,Long.y,dist),]




# gets the Bin Status field from the original data set
gpsxb <- merge(
    gpsx
    ,dt[,.(Number,Street,Zip,BinStatus),]
    ,by.x=c("Number.y", "Street.y", "Zip.y")
    ,by.y=c("Number", "Street", "Zip")
)

(gpsxb)






library(geosphere)
distm(c(39.081434, -77.233082), c(39.082399, -77.230161), fun=distGeo) #326.9657
#distm(c(39.081434, -77.233082), c(39.082399, -77.230161), fun=distHaversine) #326.0298








