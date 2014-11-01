makeLocalRank <- function(num) {
  result <- function(outcomes) {
    
    ranks <- order(outcomes$value, outcomes$name, na.last = NA)
    
    state <- as.character(outcomes$state[1])
    res <- outcomes[ranks,]
    if(num == "best") {
      rank <- 1
    } else if(num == "worst") {
      rank <- length(res$value)
    } else {
      rank <- num 
    }
    c(as.character(res$name[rank]), state, res$value[rank])
  }
  result
}


rankall <- function(outcome, num = "best") {
  if(outcome == "heart attack") {
    outcomeIndex <- 11
  } else if(outcome == "heart failure") {
    outcomeIndex <- 17
  } else if(outcome == "pneumonia") {
    outcomeIndex <- 23
  } else {
    stop("invalid outcome")
  }
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes[, outcomeIndex] <- as.numeric(outcomes[, outcomeIndex])
  data <- data.frame(state = outcomes[, 7], name = outcomes[,2], value = outcomes[,outcomeIndex])
  d <- sapply(split(data, data$state), makeLocalRank(num))
  data.frame(hospital = d[1,], state = d[2, ])
}