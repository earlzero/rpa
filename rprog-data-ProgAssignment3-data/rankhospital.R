rankhospital <- function(state, outcome, num = "best") {
  if(outcome == "heart attack") {
    outcomeIndex <- 11
  } else if(outcome == "heart failure") {
    outcomeIndex <- 17
  } else if(outcome == "pneumonia") {
    outcomeIndex <- 25
  } else {
    stop("invalid outcome")
  }

  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes[, outcomeIndex] <- as.numeric(outcomes[, outcomeIndex])
  
  stateIndexes <- outcomes[, 7] == state
  if(any(stateIndexes)) {
    subset <- outcomes[stateIndexes, ]
    ranks <- order(subset[, outcomeIndex],subset[, 2], na.last = NA)
    
    if(num == "best") {
      rank <- 1
    } else if(num == "worst") {
      rank <- length(ranks)
    } else {
      rank <- num 
    }
    subset[ranks, 2][rank]
  } else {
    stop("invalid state")
  }
}