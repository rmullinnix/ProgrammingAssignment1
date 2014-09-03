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
  files <- paste(directory, "/", formatC(id, width = 3, flag = "0"), ".csv", sep="")
  
  data.list <- lapply(files, read.csv)
  data.cat <- do.call(rbind, data.list)
  
  round(mean(data.cat[pollutant][!is.na(data.cat[pollutant])]), 3)
}