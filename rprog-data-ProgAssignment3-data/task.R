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
  stateIndexes = outcomes[, 7] == state
  if(any(stateIndexes)) {
    index <- which.min(outcomes[stateIndexes, outcomeIndex])
    outcomes[index, 2]
  } else {
    stop("invalid state")
  }
  
  
  outcomeIndex
#  stateOutcomes[index,2]
  
}

best("TX", "heart attack")
