# Write a function that takes a directory of data files and a threshold for 
# complete cases and calculates the correlation between sulfate and nitrate for 
# monitor locations where the number of completely observed cases (on all 
# variables) is greater than the threshold. The function should return a vector 
# of correlations for the monitors that meet the threshold requirement. If no 
# monitors meet the threshold requirement, then the function should return a 
# numeric vector of length 0. A prototype of this function follows

corr <- function(directory, threshold = 0) {
    
    ## Set up output and file list
    result <- numeric()
    fdir <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    
    for(floc in fdir) {
        
        ## Loop through data and locate NAs
        dat <- read.csv(floc)
        obs <- complete.cases(dat$sulfate, dat$nitrate)
        
        
        ## Calculate the correlation where possible and append to output
        if (length(obs[obs == TRUE]) > threshold) {
            corSulfate <- dat$sulfate[obs == TRUE]
            corNitrate <- dat$nitrate[obs == TRUE]
            result <- append(result, cor(corSulfate, corNitrate))
        }
        
    }
    
    ## Return a numeric vector of the resulting correlations
    result
}
