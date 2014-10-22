pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  if(!file.exists(directory) | !file.info(directory)$isdir) {
    stop(paste("The directory ", directory, "doesn't exist"))
  }    
  
  pollutant <- match.arg(pollutant,  c('sulfate', 'nitrate'))  
  
  if (any(id <= 0) | any(id > 332)) {
    stop("The monitor ID is a vector between 1 and 332")
  }
  
  files <- file.path(directory, sprintf("%03d.csv", id))
  data <- lapply(files, read.csv)
  data <- do.call(rbind.data.frame, data)
  result <- mean(data[, pollutant], na.rm = TRUE)
  
  format(result, digits=3, nsmall=3)
}