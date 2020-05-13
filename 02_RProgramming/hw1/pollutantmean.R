#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. The function '
#pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#particulate matter data from the directory specified in the 'directory' 
#argument and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## Setting up tally variables for the final calculation after looping
    totalDataPoints <- 0
    runningMean <- 0
    
    ## Loop through specified file ID's, and get the required data
    for(loc in id) {
        filePath <- file.path(directory, paste(sprintf("%03d", loc), ".csv", 
                                               sep = ""))
        dat <- read.csv(filePath)
        pollData <- dat[pollutant][[1]]
        len <- length(na.omit(pollData))
        locMean <- mean(pollData, na.rm = TRUE)
        
        ## Running mean and total points are calculated each loop to avoid
        ## creating a potentially giant monster dataframe
        if(len > 0) {
            totalDataPoints <- totalDataPoints + len
            runningMean <- runningMean + (locMean*len)
        }
        
    }
    
    ## Return the overall mean of the pollutant
    runningMean / totalDataPoints
    
}
