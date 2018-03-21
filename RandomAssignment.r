
# setwd("C:/Users/cyhu/Desktop/w241experiment/data")
# read the file with all subjects (with 'No' of bin status before treatment)
d <- read.csv("Recycling_Subjects.csv", header = T)
head(d)

# clustering randomization at street level
set.seed(1234)
library(Hmisc)
describe(d$Street) #25 distinct valus, 186 records
cluster.id <- unique(d$Street)
treat.street.ids <- factor(sample(cluster.id,13))
d$Assigned <- d$Street %in% treat.street.ids
describe(d$Assigned)

# save the assignment to csv file
write.csv(d, "Recycling_Assigned.csv")
