corr <- function(directory, threshold = 0) {
    files <- complete(directory, 1:332)
    id <- files[files$nobs > threshold,]$id
    data <- sapply(list.files(directory, full.names=T), function(name) {
        file <- read.csv(name)
        sapply(id, function(n){
            f <- file[file$ID == n & !is.na(file$nitrate) & !is.na(file$sulfate), ]          
            if(length(f$sulfate) != 0) {
                cor(f$sulfate,f$nitrate)
            } else {
                NA
            }
            
        })
    })
    data <- data[!is.na(data)]
    if(length(data[sapply(data, function(c) {length(c)!=0})]) == 0) {
        numeric(0)
    } else {
        data
    }
}