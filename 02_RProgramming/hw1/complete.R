# Write a function that reads a directory full of files and reports the number 
# of completely observed cases in each data file. The function should return a 
# data frame where the first column is the name of the file and the second column
# is the number of complete cases. A prototype of this function follows

complete <- function(directory, id = 1:332) {
    
    ## Create a labeled, empty dataframe (0x2)
    df <- data.frame(id = integer(), nobs = integer())
    
    for(loc in id) {
        
        ## Loop through the specified file IDs and generate the non-NA length
        filePath <- file.path(directory, paste(sprintf("%03d", loc), ".csv", 
                                               sep = ""))
        dat <- read.csv(filePath)
        obs <- complete.cases(dat$sulfate, dat$nitrate)
        newRow <- data.frame(id = loc, nobs = length(obs[obs == TRUE]))
        
        ## Append the new data to the master frame
        df <- rbind(df, newRow)
        
    }
    
    ## Return the completed dataframe
    df
}
