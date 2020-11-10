## rankhospital is a function that takes three arguments: the 2-character
## abbreviated name of a state (state), an outcome (outcome), and the ranking
## of a hospital in that state for that outcome (num), then returns  the name
## of the hospital that has the ranking specified by the num argument. 
rankhospital <- function(state, outcome, num = "best") {
    states <- c("AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
    diseases <- c("heart attack", "heart failure", "pneumonia")
    # column number of Hospital 30-Day Death (Mortality) Rates for 
    # Heart Attack, Heart Failure, Pneumonia respectively
    cols <- c(11, 17, 23)
    cN <-  grep("Hospital.Name", colnames(dat))  # column index of 'Hospital.Name'
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")    
    for (i in cols) {
        dat[, i] <- as.numeric(dat[, i])
    }
    ## Check that state and outcome are valid
    if (!any(state == states)) {
        stop("invalid state")
    }
    if (!any(outcome == diseases)) {
        stop("invalid outcome")
    }
    cD <-  cols[diseases == outcome]
    # In order to get number of hospitals in the state, we take dat including NA 
    # in the disease, because the specification says "If the number given by num is larger than the number of hospitals in that
    # state, then the function should return NA."
    dat0 <- dat[dat$State==state, ]
    dat1 <- dat[dat$State==state & !is.na(dat[cD]), ]
    if (num == "best") {
        nm = 1
    } else if (num == "worst") {
        nm = nrow(dat1)
    } else if (is.numeric(num) && 0<= num && num <= nrow(dat)) {
        nm = num
    } else {
        return(NA)
    }
    ## Return hospital name in that state with the given rank 30-day death rate
    # sort in ascending order of outcome, then name
    dat1[order(dat1[,cD], dat1[,cN]), ][nm,cN]
}