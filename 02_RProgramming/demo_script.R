dat <- read.csv("hw1_data.csv")
#Extract the subset of rows of the data frame where Ozone values are 
#above 31 and Temp values are above 90. What is the mean of Solar.R 
#in this subset?

mySub <- dat$Ozone > 31 & dat$Temp > 90
solarSet <- dat$Solar.R[mySub]
bad <- is.na(solarSet)
result <- mean(solarSet[!bad])
