# from Alex.h in Slack
# https://ucbischool.slack.com/archives/G8SBV2LN6/p1522804241000289
res <- list()

power_function <- function(num_subjects, control_mean, control_sd,
                           treat_mean, treat_sd) { 
    ## this is a little demo function to create a power test
    ## we're making some data; then we're going to see what kind
    ## of power we generate. 
    y0 <- rnorm(n=num_subjects/2, mean = control_mean, sd = control_sd)
    y1 <- rnorm(n=num_subjects/2, mean = treat_mean, sd = treat_sd)
    
    t.test(y0, y1)$p.value
}

iterator <- seq(10, 1000, 10)
for(i in 1:length(iterator)) {
    tmp <- replicate(
        100, 
        power_function(iterator[i], control_mean = 10, treat_mean = 10.5,
                       control_sd = 2, treat_sd = 2.2)
    )
    res[i] <- mean(tmp < 0.05)
}
plot(res, type = 'l', lty=3, col = "red")




rnorm(n=1000/2, mean = 50, sd = 5) #list of numbers
rnorm(n=1000/2, mean = 52, sd = 6) #list of numbers

t.test(rnorm(n=1000/2, mean = 50, sd = 5), rnorm(n=1000/2, mean = 52, sd = 6))$p.value #1.373194e-09

replicate( 100, 1.373194e-09 )








# from Alex.h in office hours
power_function2 <- 









# from cynthiahu
# https://ucbischool.slack.com/files/U3JDJR6GJ/FA0RB6V4G/power_calculation.txt
#install.packages("stats")
library(stats)

power.prop.test(p1 = 0, p2 = 0.1, sig.level=.05, power=0.80, alternative = c("one.sided"),)