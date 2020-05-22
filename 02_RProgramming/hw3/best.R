## best() returns the "best hospital" in the requested state in the US DHHS 
## outcome-of-care-measures.csv, by the specified outcome metric.
## Args:
## state = Valid US state 2-letter character code (50 states + DC, PR, GU, VI)
## outcome = Desired outcome metric. Must be one of "pneumonia", "heart attack",
##  or "heart failure".
best <- function(state, outcome) {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    if(is.na(match(state, dat$State))) {
        stop("invalid state")
    }
    
    ## Subset to state and evaluate outcome selection
    stateData <- split(dat, dat$State)
    if(outcome == "pneumonia") {
        ## column 23
        hosp <- get_best_outcome(data.frame(stateData[state]), 23)
    }else if(outcome == "heart attack") {
        ## column 11
        hosp <- get_best_outcome(data.frame(stateData[state]), 11)
    }else if(outcome == "heart failure") {
        ## column 17
        hosp <- get_best_outcome(data.frame(stateData[state]), 17)
    }else {
        ## error out if outcome is invalid
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    hosp
}


## get_best_outcome() does the comparison to find the hospital in the specified
## data with the minimum ("best") outcome.
## Args:
## stateOutcome = dataframe containing data, assumed filtered to a specific state
## column = numeric identifying column to find the minimum outcome
get_best_outcome <- function(stateOutcome, column) {
    
    ## Convert to numeric and get indexes of min values
    num <- suppressWarnings(as.numeric(stateOutcome[,column]))
    mins <- (which(num == min(num, na.rm = TRUE)))
    
    ## Return hospital corresponding min outcome. if tied, min() takes first 
    ## hospital in alphabetical order.
    min(stateOutcome[,2][mins], na.rm = TRUE)
    
}
