## rankhospital() returns the hospital in the requested state and outcome metric
## with the rank requested.
## Args:
## state = Valid US state 2-letter character code (50 states + DC, PR, GU, VI)
## outcome = Desired outcome metric. Must be one of "pneumonia", "heart attack",
##  or "heart failure".
## num = an integer value indicating the rank for that state, or the character
##  strings "best" or"worst".
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv")

    ## Check that state is valid
    if(is.na(match(state, dat$State))) {
        stop("invalid state")
    }
    
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
    
    
    ## Subset to state and evaluate outcome selection
    stateData <- data.frame(split(dat, dat$State)[state])
    if(outcome == "pneumonia") {
        ## column 23
        processed <- sort_data(stateData, 23)
        #sorted <- stateData[order(stateData[[23]], stateData[[2]]),]
    }else if(outcome == "heart attack") {
        ## column 11
        processed <- sort_data(stateData, 11)
        #sorted <- stateData[order(stateData[[11]], stateData[[2]]),]
    }else if(outcome == "heart failure") {
        ## column 17
        processed <- sort_data(stateData, 17)
        #sorted <- stateData[order(stateData[[17]], stateData[[2]]),]
    }else {
        ## error out if outcome is invalid
        processed <- stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    processed[rank(processed), 2]
    
}


sort_data <- function(dat, column) {
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
    