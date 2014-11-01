pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  data <- sapply(list.files(directory, full.names=T), function(name) {
    file <- read.csv(name)
  })
  
  result <- numeric(0)
  data <- data[pollutant, id]
  for(d in data) {
    result <- c(result, d)
  }
  mean(result, na.rm = T)
}

