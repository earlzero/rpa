complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the cc[monitor ID number and 'nobs' is the
    ## number of complete cases
    
    #    res <- sapply(list.files(directory, full.names = T),
    #                  function(name) {
    #                          file <- read.csv(name)
    #                          index <- apply(file, 1, function(n) {
    #                              !is.na(n[2]) & !is.na(n[3])
    #                          })
    #                          file[index, ]$ID
    #                  }
    #    )

    #    res
    ##    c <- do.call("rbind", res)
    ##  names(c) <- c("id", "nobs")
    ##    c
    directory <- "specdata"
    res <- sapply(list.files(directory, full.names=T), function(name) {
        file <- read.csv(name)
        index <- apply(file, 1, function(n) {
            !is.na(n[2]) & !is.na(n[3])
        })
        file[index, ]$ID
    })
    c <- do.call("rbind", lapply(id, function(i) {
        data.frame(i, sum(sapply(res,function(n) {length(n[n==i])})))
    }))
    names(c) <- c("id", "nobs")
    ##    c <- data.frame(as.integer(as.character(c$id)), c$nobs, stringsAsFactors = F)
    ##    names(c) <- c("id", "nobs")
    c
}



