best <- function(state, outcome) {
    
    myColName <- paste("Hospital 30-Day Death (Mortality) Rates from", outcome)
    outcomeList <- tolower(c("Heart Attack", "Heart Failure", "Pneumonia"))

    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (state %in% unique(outcomeData$State)) {
        if (outcome %in% outcomeList){
            
            if (tolower(outcome) == "heart attack") {
                columnToUse <- 11
            } else if (tolower(outcome) == "heart failure"){
                columnToUse <- 17
            } else if (tolower(outcome) == "pneumonia") {
                columnToUse <- 23
            }
            outcomeData[, columnToUse] <- as.numeric(outcomeData[, columnToUse])
            stateDataTemp <- outcomeData[outcomeData$State == state,]
            stateData <- stateDataTemp[!is.na(stateDataTemp[,columnToUse]),]
            minDeathHospital <- which.min(tapply(stateData[, columnToUse], 
                                        stateData$Hospital.Name, 
                                        sum))
            return (names(minDeathHospital))
      
        } else {
            print("invalid outcome")
            return (NULL)
        }
    } else {
        print("invalid state")
        return (NULL)
    }
}

rankhospital <- function(state, outcome, num = "best") {
    myColName <- paste("Hospital 30-Day Death (Mortality) Rates from", outcome)
    outcomeList <- tolower(c("Heart Attack", "Heart Failure", "Pneumonia"))
    
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (state %in% unique(outcomeData$State)) {
        if (outcome %in% outcomeList){
            
            if (tolower(outcome) == "heart attack") {
                columnToUse <- 12
            } else if (tolower(outcome) == "heart failure"){
                columnToUse <- 18
            } else if (tolower(outcome) == "pneumonia") {
                columnToUse <- 24
            }
            outcomeData[, columnToUse-1] <- as.numeric(outcomeData[, columnToUse-1])
            stateDataTemp <- outcomeData[outcomeData$State == state,]
            stateData <- stateDataTemp[!is.na(stateDataTemp[,columnToUse-1]),]
            
            HospitalStateDeath <- tapply(stateData[, columnToUse-1], 
                                                 stateData$Hospital.Name, 
                                                 sum)
            HospitalStateDeath <- HospitalStateDeath[order(HospitalStateDeath)]
            if (num =="best"){
                return (names(head(HospitalStateDeath,1)))
            }else if (num =="worst"){
                return (names(tail(HospitalStateDeath,1)))
            }else{
                return (names(HospitalStateDeath[num]))
            }
            
        } else {
            print("invalid outcome")
            return (NULL)
        }
    } else {
        print("invalid state")
        return (NULL)
    }
}

rankall <- function(outcome, num = "best") {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    stateList <- unique(outcomeData$State)
    returnState <- c()
    returnHospital <- c()
    for (state in stateList) {
        returnState <- c(returnState, state)
        returnHospital <- c(returnHospital, rankhospital (state, outcome, num))
    }
    DF <- data.frame(returnState, returnHospital)
    colnames(DF) <- c("State", "Hospital")
    return(DF)
}
