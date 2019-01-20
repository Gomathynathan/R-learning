##Set your working directory here
hospital <- read.csv("hospital-data.csv", colClasses = "character")
outcome <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11]
hist(as.numeric(outcome[, 11]))


best <- function(state, out) {
    outcome1 <- outcome[, c(1:9, 13, 19, 25)]
    
    
    if (out == "heart attack")
        x <- 10
    else if (out == "heart failure")
        x <- 11
    else if (out == "pneumonia")
        x <- 12
    else
        return("Invalid outcome")
    
    hlist <- outcome1[outcome1$State == state, ]
    if (nrow(hlist) == 0)
        return("Invalid state")
    
    y <- hlist$Hospital.Name[which.min(hlist[, x])]
    
    return(y)
}



rankhospital <- function(state, out, ranknum) {
    outcome1 <- outcome[, c(1:9, 11, 17, 23)]
    hlist <- outcome1[outcome1$State == state, ]
    
    if (out == "heart attack") {
        x <- 10
        hlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[hlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
                                                                            "Not Available"] <- NA
    }
    else if (out == "heart failure") {
        x <- 11
        hlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[hlist$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
                                                                             "Not Available"] <- NA
    }
    else if (out == "pneumonia") {
        x <- 12
        hlist$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[hlist$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
                                                                         "Not Available"] <- NA
    }
    else
        return("Invalid outcome")
    
    if (nrow(hlist) == 0)
        return("Invalid state")
    
    hlistnew <- hlist[complete.cases(hlist[, x]), ]
    zz1 <-
        hlistnew[order(as.numeric(hlistnew[, x]), hlistnew[, 2], decreasing = FALSE), ]
    if (ranknum == "worst")
        return(zz1$Hospital.Name[c(nrow(zz1))])
    else if (ranknum == "best")
        return(zz1$Hospital.Name[1])
    else
        (return(zz1$Hospital.Name[ranknum]))
}

rankall <- function(out, numb = "best") {
    outcome1 <- outcome[, c(1:9, 11, 17, 23)]
    
    if (out == "heart attack") {
        x <- 10
        outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
                                                                               "Not Available"] <- NA
    }
    else if (out == "heart failure") {
        x <- 11
        outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
                                                                                "Not Available"] <- NA
    }
    else if (out == "pneumonia") {
        x <- 12
        outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
                                                                            "Not Available"] <- NA
    }
    else
        return("Invalid outcome")
    aablist <- character()
    aablist2 <- character()
    outcomenew <- outcome1[complete.cases(outcome1[, x]), ]
    st <- sort(unique(outcomenew$State))
    stlen <- length(st)
    for (i in 1:stlen) {
        state_subset <- outcomenew[outcomenew[, 7] == st[i], ]
        aab <-
            state_subset[order(as.numeric(state_subset[, x]),
                               state_subset[, 2],
                               decreasing = FALSE), ]
        aablist2[i] <- st[i]
        
        if (numb == "worst") {
            aablist[i] <- aab$Hospital.Name[c(nrow(aab))]
        }
        else if (numb == "best") {
            aablist[i] <- aab$Hospital.Name[1]
        }
        else{
            aablist[i] <- aab$Hospital.Name[c(numb)]
            
        }
    }
    result <- as.data.frame(cbind(aablist, aablist2))
    colnames(result) <- c("hospital", "state")
    return(result)
}
