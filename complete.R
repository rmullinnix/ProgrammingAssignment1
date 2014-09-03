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
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  files <- paste(directory, "/", formatC(id, width = 3, flag = "0"), ".csv", sep="")
  
  data.list <- lapply(files, read.csv)
  data.cat <- do.call(rbind, data.list)
  
  ids <- unique(data.cat$ID)
  compl <- data.cat[complete.cases(data.cat),]
  nobs <- tapply(compl$ID, compl$ID, FUN = length)
  nobs <- nobs[as.character(ids)]
  result <- data.frame(ids, nobs)
  row.names(result) <- NULL

  result
}