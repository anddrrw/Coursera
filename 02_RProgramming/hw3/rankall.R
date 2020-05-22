## rankall() returns the hospital of the requested rank and outcome metric in each
## state, as a dataframe containing the hospital name and state.
## Args:
## outcome = Desired outcome metric. Must be one of "pneumonia", "heart attack",
##  or "heart failure".
## num = an integer value indicating the rank for that state, or the character
##  strings "best" or"worst".
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv")
    df_output <- data.frame(hospital = character(), state = character())
    
    ## Check that num is valid. If user calls worst, will need length of final
    ## sorted dataframe
    if(num == "best") {
        rank <- function(x) 1
    }else if(num == "worst") {
        rank  <-  function(x) nrow(x)
    }else if(is.numeric(num)){
        rank <- function(x) num
    }else{
        stop("invalid rank num")
    }
    
    ## Prepare data. Organize dat by state, and get an ordered list of states
    stateFrames <- split(dat, dat$State)
    states <- unique(dat$State)[order(unique(dat$State))]
    
    ## Loop through each state and get ranked hospital. Add to final df.
    for(state in states) {
        ## Subset to state and evaluate outcome selection
        stateData <- data.frame(stateFrames[state])
        if(outcome == "pneumonia") {
            ## column 23
            processed <- sort_data(stateData, 23)
        }else if(outcome == "heart attack") {
            ## column 11
            processed <- sort_data(stateData, 11)
        }else if(outcome == "heart failure") {
            ## column 17
            processed <- sort_data(stateData, 17)
        }else {
            ## error out if outcome is invalid
            processed <- stop("invalid outcome")
        }
        ## Append the state's ranked hospital to the final output df
        newRow <- data.frame(hospital = processed[rank(processed), 2], state = state)
        df_output <- rbind(df_output, newRow)
        
    }
    
    df_output
}

sort_data <- function(dat, column) {
    
    ## This is some messy shit right here. And I hate it. So here are comments.
    ## Sort state data based on column, and then by hospital name. If data 
    ## isn't available for a hospital, it will produce an NA here.
    sorted <- suppressWarnings(dat[order(as.numeric(dat[[column]]), dat[[2]]),])
    
    ## In the sorted table, all NAs are always sorted to the bottom. 
    ## Need to find the index of the first NA in order to return a truncated
    ## table, with no NAs.
    NAs <- suppressWarnings(which(is.na(as.numeric(sorted[[column]]))))
    if(length(NAs) > 0) {
        sorted <- sorted[1:NAs[1] - 1,]
    }
    sorted
    
}