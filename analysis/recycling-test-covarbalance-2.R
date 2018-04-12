# this is just a test script for checking covarate balance. To be merged into main test script.

getwd()
setwd("C:/Users/yangyq/workspaces/ucbiyyq/w241experiment/analysis")
getwd()


# loads raw data
dt <- data.table(read.csv("../data/Data - BOTH.csv", na.strings=c("")))

# creates some dummy variables for ease of query and analysis later
dtA <- dt[
    ,.(
        Number=factor(Number)
        ,Street=factor(Street)
        ,PickupDOW
        ,Pre
        ,Treat
        ,Post
        ,S=factor(ifelse(Pre=="N",1,0))
        ,D=ifelse(Treat=="Y",1,0)
        ,pre.bin=ifelse(Pre=="Y",1,0)
        ,post.bin=ifelse(Post=="Y",1,ifelse(Post=="N",0,NA))
    )
    ,
    ] 
# ignoring Route and Zip because those values do not change


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



# covariates we should check for:
# street.score
# PickupDOW
# neighbor.score TBD

# see http://people.ischool.berkeley.edu/~qianyu/my_ds_projects/W241_project_code_final.html

m.cbc.1 <- lm(Treat ~ street.score, data=dtA[S==1])
(m.cbc.1)
#plot(m.cbc.1)


m.cbc.2 <- lm(Treat ~ PickupDOW, data=dtA[S==1])
(m.cbc.2)
#plot(m.cbc.2)


stargazer(m.cbc.1, m.cbc.2, type="text", keep.stat=c("n"), title="Covariate Balance Check")




m.cbc.3 <- lm(Treat ~ PickupDOW + street.score, data=dtA[S==1])
(m.cbc.3)
#plot(m.cbc.3)

#m.cbc.3$vcovHC <- vcovHC(m.cbc.3)
#coeftest(m.cbc.3, m.cbc.3$vcovHC)

stargazer(m.cbc.3, type="text", title="Covariate Balance Check")



