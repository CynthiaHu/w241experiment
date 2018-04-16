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
        ,City
        ,State
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
        ,post.bin.a=ifelse(is.na(Post),0,ifelse(Post=="Y",1,0))
        ,post.bin.b=ifelse(is.na(Post),1,ifelse(Post=="Y",1,0))
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
        ,n.pre.Y=ifelse(is.na(n.pre.Y),0,n.pre.Y)
        ,n.houses
    )
    ,
][
    ,.(
        Street
        #,n.pre.Y
        #,n.houses
        ,street.score=n.pre.Y/n.houses
    )
    ,
]


# joins the street score data table back into the main data table
dtA <- merge( x=dtA, y=s.scr, by.x="Street", by.y="Street", all.x=TRUE )










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


# note, just the number, street, and zip works most of the time, but sometimes generate multiple guesses
#https://maps.googleapis.com/maps/api/geocode/json?address=13602+Pine+View+Lane+20850&key=AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM
# returns 3 guesses
#https://maps.googleapis.com/maps/api/geocode/json?address=13602+Pine+View+Lane+Rockville+MD+20850&key=AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM
# returns 1 guess


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





# ==================================================================================
# this section of code calls the google geolocation api for all houses
# see https://cran.r-project.org/web/packages/urltools/vignettes/urltools.html
library(httr)
library(jsonlite)
#install.packages("urltools")
library(urltools)

ggeo <- function(num, str, city, state, zip) {
    u <- "https://maps.googleapis.com/maps/api/geocode/json"
    u <- param_set(u, key = "address", value = gsub(" ","+",paste(num, str, city, state, zip)))
    u <- param_set(u, key = "key", value = "AIzaSyAvdbge4u05DJ7pD9rgQq0NC3O6SHl3DTM")
    r <- GET(u)
    rc <- content(r,"text")
    rjs <- fromJSON(rc)
    lat.lng <- rjs$results$geometry$location
    return(paste(lat.lng$lat, lat.lng$lng))
}

# calls the google geolocation API for every address in the data set
setkey(dtA,Number,Street,City,State,Zip)
gps <- dtA[
    ,.(
        lat.lng=ggeo(Number,Street,City,State,Zip)
        )
    ,by=c("Number","Street","City","State","Zip")
][
    ,c("lat","lng"):=tstrsplit(lat.lng," ",fixed=TRUE)
    ,
][
    ,.(Number,Street,City,State,Zip,lat,lng)
]

# saves the gps results to disk
fwrite(gps,"../data/Data-gps.csv", quote="auto", sep=",", eol="\n", na="")

# for some reason neither the geolocation api nor google maps can find these addresses
# 11401 Cromwood Road Parkville MD 20850
# 11403 Cromwood Road Parkville MD 20850
# 11405 Cromwood Road Parkville MD 20850
# 11407 Cromwood Road Parkville MD 20850
# 11409 Cromwood Road Parkville MD 20850
# 11411 Cromwood Road Parkville MD 20850
# 11413 Cromwood Road Parkville MD 20850


# ==================================================================================









# ==================================================================================
# reads gps from disk, prefetched by the test-gps script
gpsr <- data.table(read.csv("../data/Data-gps.csv", na.strings=c("")))[
    ,.(
        Number=factor(Number)
        ,Street
        ,City
        ,State
        ,Zip=factor(Zip)
        ,lat
        ,lng)
    ,
]


# calculates the cross-join of each house to others
gpsj <- gpsr[,.(Number, Street, Zip, lat, lng, j=1),] #adds a dummy column "j" to manipulate the cross join
gpsx <- merge(gpsj, gpsj, by='j', all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)


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


# dt.geodist <- function(lat_from=0, lon_from=0, lat_to=0, lon_to=0) {
#     return(distm( c(lat_from, lon_from), c(lat_to, lon_to), fun=distGeo ))
# }


# calculates all the distances between all houses
#library(geosphere)
gpsx <- gpsx[ 
    , .(
            Number.x=factor(Number.x)
            ,Street.x=factor(Street.x)
            ,Zip.x=factor(Zip.x)
            ,lat.x
            ,lng.x
            ,Number.y=factor(Number.y)
            ,Street.y=factor(Street.y)
            ,Zip.y=factor(Zip.y)
            ,lat.y
            ,lng.y
            ,dist=dt.haversine(lat.x, lng.x, lat.y, lng.y)
            #,num.diff=Number.x-Number.y
            #,dist.lib=distm( c(lat.x, lng.x), c(lat.y, lng.y), fun=distHaversine )
            #,dist.lib=dt.geodist( c(lat.x, lng.x), c(lat.y, lng.y) )
            ,num.diff = as.numeric(levels(Number.x))[Number.x] - as.numeric(levels(Number.y))[Number.y]
        )
    , 
]


# left joins the dtA data to the gpsx data, specifically on y-columns, so that we can calculate the neighbor score of the x-columns
setkey(gpsx,Number.y,Street.y)
setkey(dtA,Number,Street)
gpsx <- merge(
    x=gpsx
    ,y=dtA
    ,by.x=c("Number.y","Street.y")
    ,by.y=c("Number","Street")
    ,all.x=TRUE
    ,allow.cartesian=TRUE 
)[
    ,.(
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
        ,Pre.y=Pre
        ,pre.bin.y=pre.bin
        ,PickupDOW.y=PickupDOW
        ,wed.y=wed
        ,dist
        ,num.diff
    )
]


# calcuates the neighbor score, as ratio of Pre==Y / Number of neighbors, per house
# neighbors defined as houses within 60 meters, but the house itself
# note, due to some addresses that can't be found, there will be some missing houses
n.scr <- merge(
    x=gpsx[dist>0 & dist<60 & Pre.y=="Y", .(n.pre.y=.N), by=c("Number.x","Street.x")]
    ,y=gpsx[dist>0 & dist<60, .(n.neighbors=.N), by=c("Number.x","Street.x")]
    ,by.x=c("Number.x","Street.x")
    ,by.y=c("Number.x","Street.x")
    ,all.y=TRUE
)[
    ,.(
        Number=Number.x
        ,Street=Street.x
        ,n.pre.y=ifelse(is.na(n.pre.y),0,n.pre.y)
        ,n.neighbors
    )
    ,
][
    ,.(
        Number
        ,Street
        #,n.pre.y
        #,n.neighbors
        ,neighbors.score=n.pre.y/n.neighbors
    )
    ,
]


# joins the street score data table back into the main data table
dtA <- merge(
    x=dtA
    ,y=n.scr
    ,by.x=c("Number","Street")
    ,by.y=c("Number","Street")
    ,all.x=TRUE 
)



# joins the gps data back into main data table for future visualizations
dtA <- merge(
    x=dtA
    ,y=gpsr[,.(Number,Street,lat,lng)]
    ,by.x=c("Number","Street")
    ,by.y=c("Number","Street")
    ,all.x=TRUE 
)


# selects just the columns we care about
# replaces any neighbor score NA with 0
dtA <- dtA[
    ,.(
        Number
        ,Street
        ,City
        ,State
        ,Zip
        ,Route
        ,PickupDOW
        ,Pre
        ,Treat
        ,Post
        ,S
        ,D
        ,pre.bin
        ,post.bin
        ,post.bin.a
        ,post.bin.b
        ,wed
        ,street.score
        ,neighbors.score=ifelse(is.na(neighbors.score),0,neighbors.score)
        ,lat
        ,lng
    )
    ,
]


# ==================================================================================


#gpsx[,.(lat.x, lng.x, lat.y, lng.y)][order(lat.x)]
#gpsx[,.(lat.x, lng.x, lat.y, lng.y, a=distGeo(matrix(c(lat.x, lng.x), ncol=2)))]
#gpsx[,.(lat.x, lng.x, lat.y, lng.y, vector(c(lat.x, lng.x)))]
#distm(gpsx[,.(c(lat.x, lng.x))],gpsx[,.(c(lat.y, lng.y))],fun=distGeo)
#gpsx[,.(lat.x, lng.x, lat.y, lng.y, distGeo(matrix(c(lat.x, lng.x), ncol=2),c(0, 0)))]
#gpsx[,.(lat.x, lng.x, lat.y, lng.y, distGeo(matrix(c(39.09563, -77.21698), ncol=2),c(0, 0)))]
#gpsx[,.(lat.x, lng.x, lat.y, lng.y, distGeo(vector(c(lat.x, lng.x)),c(0, 0)))]
#matrix(c(0, 0), ncol=2)
# can't seem to get the geosphere library functions to work, so will need to use the custom function







# ==================================================================================
# some basic analysis to callibrate which dist cutoff we should use to determine "neighbors"

# finds the street with the highest and lowest houses
dtA[,.N,by=Street][order(N)]
# Quietwood Drive  2
# Potomac Oaks Drive 82


# eyeball check of gps data for every house
summary(gpsr[,.(lat,lng)])
hist(gpsr[,.(lat)]$lat)
hist(gpsr[,.(lng)]$lng)

# distribution of the distances between houses
summary(gpsx$dist)
hist(gpsx$dist, breaks=1000)
hist(log(gpsx$dist), breaks=1000)
hist(gpsx[dist>=50]$dist)
hist(gpsx[dist<50]$dist)

# distribution of the distances between houses, for our street with the most houses
summary(gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive"]$dist)
hist(gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive"]$dist)

# distribution of the distances between houses, for our street with the most houses, for the closer houses
hist(gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist>0 & dist<100]$dist, breaks=rep(0:100))
gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist>0 & dist<100][order(dist)]

hist(gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist>0 & dist<60]$dist, breaks=rep(0:60))
gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist>0 & dist<60][order(dist)]

hist(gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist > 60 & dist < 70]$dist, breaks=rep(60:70))
gpsx[Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist>60 & dist<70][order(dist)]
# eyeball check on google maps: somewhere between 60 and 70 meters on our longest street is the the distance that means between two to three houses apart

gpsx[
    Street.x=="Potomac Oaks Drive" & Street.y=="Potomac Oaks Drive" & dist>0 & dist<100
    ,plot(
        dist
        ,abs(num.diff)
        ,main="Potomac Oaks Drive"
        ,xlab="dist"
        ,ylab="abs of num.diff"
    )
]
# the street number diff is less reliable, because in suburban streets, the houses that share backyards don't share similar street numbers
# no clear correlation, even just on one street
# e.g. 11407 Potomac Oaks Drive 20850 vs 11421 Potomac Oaks Drive 20850, has dist of 60.70061 and abs of num diff of 14, but on map are basically just two backyards away from each other



# distribution of the distances between houses, for our street with the least houses
summary(gpsx[Street.x=="Quietwood Drive" & Street.y=="Quietwood Drive" & dist>0]$dist)
hist(gpsx[Street.x=="Quietwood Drive" & Street.y=="Quietwood Drive" & dist>0]$dist)
# there only two houses on the short street in our records, and they are 30 meters apart
# ==================================================================================






# see https://www.rdocumentation.org/packages/ggforce/versions/0.1.1/topics/facet_wrap_paginate


for (i in unique(dtA[,Street])) {
    plt <- ggplot(data=dtA[Street==i,.(lat,lng,Treat,S)], mapping=aes(x=lng,y=lat,color=Treat,shape=S)) +
        geom_point(size=5) + 
        ggtitle(label=paste(i)) +
        xlab("Longitude") +
        ylab("Latitude")
    print(plt)
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








