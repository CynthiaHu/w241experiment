# this is just a test script for statistical power simulations. To be merged into main test script.
# import our data from the experiment

# alternative: could do this without the data, just back of napkin calculations

getwd()
setwd("C:/Users/yangyq/workspaces/ucbiyyq/w241experiment/analysis")
getwd()

#install.packages("data.table")
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








# # from Alex.h in Slack
# # https://ucbischool.slack.com/archives/G8SBV2LN6/p1522804241000289
# res <- list()
# 
# power_function <- function(num_subjects, control_mean, control_sd,
#                            treat_mean, treat_sd) {
#     ## this is a little demo function to create a power test
#     ## we're making some data; then we're going to see what kind
#     ## of power we generate.
#     y0 <- rnorm(n=num_subjects/2, mean = control_mean, sd = control_sd)
#     y1 <- rnorm(n=num_subjects/2, mean = treat_mean, sd = treat_sd)
# 
#     t.test(y0, y1)$p.value
# }
# 
# iterator <- seq(10, 1000, 10)
# for(i in 1:length(iterator)) {
#     tmp <- replicate(
#         100,
#         power_function(iterator[i], control_mean = 10, treat_mean = 10.5,
#                        control_sd = 2, treat_sd = 2.2)
#     )
#     res[i] <- mean(tmp < 0.05)
# }
# plot(res, type = 'l', lty=3, col = "red")
# 
# 
# 
# 
# rnorm(n=1000/2, mean = 50, sd = 5) #list of numbers
# rnorm(n=1000/2, mean = 52, sd = 6) #list of numbers
# 
# t.test(rnorm(n=1000/2, mean = 50, sd = 5), rnorm(n=1000/2, mean = 52, sd = 6))$p.value #1.373194e-09
# 
# replicate( 100, 1.373194e-09 )
# 
# 
# 
# 
# 
# # from cynthiahu
# # https://ucbischool.slack.com/files/U3JDJR6GJ/FA0RB6V4G/power_calculation.txt
# #install.packages("stats")
# library(stats)
# 
# power.prop.test(p1 = 0, p2 = 0.1, sig.level=.05, power=0.80, alternative = c("one.sided"),)






# from Alex.h in office hours


# 1. calculate the sample mean from treatment, and the sample mean from control
#       because our experiment doesn't have a distribution of treatment effects, we don't need to calculate the variance (our variance is essentially 1, uniform)
# 2. use the number of units in control, uniform distribution, and sample mean of control to simluate a potential outcome to control (y0)
#       could use rbinom distrubtions, (or other: counting, poisson)  instead of the rnorm distribution in Alex's previous code
# 3. use the number of units in treatment, uniform distribution, and sample mean of treatment to simluate a potential outcome to treatment (y1)
#       could use rbinom distrubtions, (or other: counting, poisson)  instead of the rnorm distribution in Alex's previous code
# 4. do a t-test on potential outcome to control, and potential outcome to treatment
#       assumes we are doing a two-tailed test, with a 95% confidence level
#       return the p-value of our t-test
# 5. repeat this simulation keeping everyting the same to get a distribution of p-values
#       look at the resulting distribution of p-values
#       calculate the power of the test as the mean of p-values < 0.05
#       this tells us essentially what our chances are of detecting a resule based on our experiment's parameters
# 6. repeat this simulation using differing number of subjects or different sample means in the treatment vs control groups


power.function.single <- function(
    sim.n.units.treat
    ,sim.n.units.control
    ,sim.mu.treat
    ,sim.mu.control
) {
    y1 = rbinom(n=sim.n.units.treat, size=1, prob=sim.mu.treat)
    y0 = rbinom(n=sim.n.units.control, size=1, prob=sim.mu.control)
    return(t.test(y0, y1)$p.value)
}


# calculate the ingredients needed for Alex's new power function
# note, when calculating ATE, we dropped the Post==NAs records, due to measurement errors

n.bins.treat.post <- dtA[S==1 & !is.na(Post) & Treat=="Y" & Post=="Y",.N] #33
n.bins.treat.pre  <- dtA[S==1 & !is.na(Post) & Treat=="Y" & Pre=="Y",.N] #0
n.units.treat     <- dtA[S==1 & !is.na(Post) & Treat=="Y", .N] #102
mu.treat          <- (n.bins.treat.post - n.bins.treat.pre) / n.units.treat #0.3235
sd.treat          <- 1 # not used for rbinom distribution

n.bins.control.post <- dtA[S==1 & !is.na(Post) & Treat=="N" & Post=="Y",.N] #34
n.bins.control.pre  <- dtA[S==1 & !is.na(Post) & Treat=="N" & Pre=="Y",.N] #0
n.units.control     <- dtA[S==1 & !is.na(Post) & Treat=="N", .N] #80
mu.control          <- (n.bins.control.post - n.bins.control.pre) / n.units.control #0.425
sd.control          <- 1 # not used for rbinom distribution

tau <- mu.treat - mu.control



# run the power function many times to get a distribution of p-values
#set.seed(42)

p.values <- replicate(1000,
    power.function.single(
        sim.n.units.treat    = n.units.treat
        ,sim.n.units.control = n.units.control
        ,sim.mu.treat            = mu.treat
        ,sim.mu.control          = mu.control
    )
)
hist(p.values, main=paste("Power = ", mean(p.values < 0.05)))
# looks like we would only correctly reject H0 when H1 is true (i.e. there is likely not no effect) about 21% of the time



p.values <- replicate(1000,
      power.function.single(
          sim.n.units.treat    = n.units.treat * 2
          ,sim.n.units.control = n.units.control * 2
          ,sim.mu.treat            = mu.treat
          ,sim.mu.control          = mu.control
      )
)
hist(p.values, main=paste("Power = ", mean(p.values < 0.05)))
# if we had twice as many subjects in both our treatment and control groups, then our experiment would have a power of about .36



p.values <- replicate(1000,
      power.function.single(
          sim.n.units.treat    = n.units.treat * 4
          ,sim.n.units.control = n.units.control * 4
          ,sim.mu.treat            = mu.treat
          ,sim.mu.control          = mu.control
      )
)
hist(p.values, main=paste("Power = ", mean(p.values < 0.05)))
# if we had four times as many subjects in both our treatment and control groups, then our experiment would have a power of about .64




p.values <- replicate(1000,
      power.function.single(
          sim.n.units.treat    = n.units.treat
          ,sim.n.units.control = n.units.control
          ,sim.mu.treat            = mu.control + .2
          ,sim.mu.control          = mu.control
      )
)
hist(p.values, main=paste("Power = ", mean(p.values < 0.05)))
# if we had a treatment mean that is 20% larger than the control, then we can reject H0 correctly 77% of the time







# layer a multiple-test simulation on top of Alex's second power test function
# see week04_powerAnalysis.Rmd
# does quite work, will need to leave out of final report

# returns mean of p.values greater than 0.05, given some simulated sample size and treatment effect
power.function.multi <- function(
    sim.n.units.treat
    ,sim.n.units.control
    ,sim.mu.treat
    ,sim.mu.control
) {
    p.values <- replicate(
        1000
        ,power.function.single(
          sim.n.units.treat = sim.n.units.treat
          ,sim.n.units.control = sim.n.units.control
          ,sim.mu.treat = sim.mu.treat
          ,sim.mu.control = sim.mu.control
        )
    )
    return(mean(p.values < 0.05))
}



# create a data table with some simulation parameters, p.value vs tau, holding sample size steady
#102
#80
#0.3235
#0.425
temp <- setDT( list( seq(mu.treat-1,mu.treat+1,0.1) ) )
temp <- setDT( list( seq(mu.control-1,mu.control+1,0.1) ) )

temp <- setDT( list( seq( tau-1, tau+1, 0.01 ) ) )


pt <- data.table(
    sim.n.units.treat=n.units.treat
    ,sim.n.units.control=n.units.control
    ,sim.mu.control=mu.control
    ,sim.tau=setDT( list( seq( tau-1, tau+1, 0.01 ) ) )
)

pt[ 
    , ':=' (sim.mu.treat=sim.mu.control+sim.tau.V1) 
]

pt

pt[
    , ':=' (
        mean.p.value=power.function.multi(
                                        sim.n.units.treat = sim.n.units.treat
                                        ,sim.n.units.control = sim.n.units.control
                                        ,sim.mu.treat = sim.mu.treat
                                        ,sim.mu.control = sim.mu.control
                                    )
            )
]

pt

pt <- pt[
    ,.(
        sim.n.units.treat
        ,sim.n.units.control
        ,sim.mu.treat
        ,sim.mu.control
        
    )
]

p.values <- replicate(
    1000
    ,power.function.single(
        sim.n.units.treat = n.units.treat
        ,sim.n.units.control = n.units.control
        ,sim.mu.treat = mu.treat
        ,sim.mu.control = mu.control
    )
)


power.function.multi(
    n=1000
    ,sim.n.units.treat    = n.units.treat
    ,sim.n.units.control = n.units.control
    ,sim.mu.treat            = mu.treat
    ,sim.mu.control          = mu.control
)




# p.value vs sample size, holding tau steady




# conclusion: talk about both
# e.g. holding the sample sizes the same, but playing with the treatment effect. Do a chart
# holding the treatment effect, but playing with the sample size

