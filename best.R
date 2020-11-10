## best is a function  that take two arguments: the 2-character abbreviated
## name of a state and an outcome name.  
best <- function(state, outcome) {
    states <- c("AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
    diseases <- c("heart attack", "heart failure", "pneumonia")
    cols <- c(11, 17, 23)
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
    cN <-  grep("Hospital.Name", colnames(dat))  # column index of 'Hospital.Name'
    ## Return hospital name in that state with lowest 30-day death rate
    dat1 <- dat[dat$State==state & !is.na(dat[cD]), ]
    dat1[order(dat1[,cD], dat1[,cN]), ][1,cN]
}