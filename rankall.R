## Ranking hospitals in all states
## rankall is a that takes two arguments: an outcome name (outcome) and
## a hospital ranking (num), returns a data frame of abbreviated state
## and the hospital of rank num in that state.
rankall <- function(outcome, num = "best") {
    diseases <- c("heart attack", "heart failure", "pneumonia")
    # column number of Hospital 30-Day Death (Mortality) Rates for 
    # Heart Attack, Heart Failure, Pneumonia respectively
    cols <- c(11, 17, 23) 
    if (num == "best") {
        nm = 1
    } else if (num == "worst") {
        nm <- NA   # temporariry set nm to NA, it is determined when dat1 is selected
    } else if (is.numeric(num) && 0<= num) {
        nm = num
    } else {
        return(NA)
    }
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")    
    cN <-  grep("Hospital.Name", colnames(dat))  # column index of 'Hospital.Name'
    for (i in cols) {
        dat[, i] <- as.numeric(dat[, i])
    }
    ## Check that state and outcome are valid
    if (!any(outcome == diseases)) {
        stop("invalid outcome")
    }
    cD <-  cols[diseases == outcome]  # column index of outcome
    state <- unique(sort(dat$State))  # what happens if a state has no hospital?
    hospital <- c()
    ## For each state, find the hospital of the given rank
    for (st in state) {
        dat1 <- dat[dat$State==st  & !is.na(dat[cD]), ]
        if (num == "worst") {nm <- nrow(dat1) }  # set nm to last row: "worst"
        if (sum(dat$State==st) < nm) {
            hospital <- c(hospital, NA)
        } else {
            hospital <- c(hospital, dat1[order(dat1[,cD], dat1[,cN]), ][nm,cN])
        }
    }
    ## Return a data frame with the hospital names and the (abbreviated) state name
    data.frame(hospital, state)
}