complete <- function(directory, id = 1:332) {
  if(!file.exists(directory) | !file.info(directory)$isdir) {
    stop(paste("The directory ", directory, "doesn't exist"))
  }    
  
  if (any(id <= 0) | any(id > 332)) {
    stop("The monitor ID is a vector between 1 and 332")
  }
  
  completedCases <- function(x)
  {  
    path = file.path(directory, sprintf("%03d.csv", x))
    sum(complete.cases(read.csv(path)))
  }
  
  data.frame(id = id, nobs = mapply(completedCases, id))
}