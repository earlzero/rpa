best <- function(state, outcome) {
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
#    outcomes[order(outcomes[stateIndexes, outcomeIndex], na.last = T),2][1]
    subset <- outcomes[stateIndexes, ]
    subset[order(subset[, outcomeIndex], subset[, 2], na.last = T),2][1]
  } else {
    stop("invalid state")
  }
  
#  stateOutcomes[index,2]
  
}

