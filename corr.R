corr <- function(directory, threshold = 0) {

  if(!file.exists(directory) | !file.info(directory)$isdir) {
    stop(paste("The directory ", directory, "doesn't exist"))
  }
  
  correlatedCases <- function(x)
  {    
    path = file.path(directory, sprintf("%03d.csv", x))
    d <- read.csv(path)
    c <- d[complete.cases(d), ]
    if (nrow(c) > threshold) {
      cor(c$nitrate, c$sulfate)
    } else {
      0
    }
  }
  v <- sapply(1:332, correlatedCases) 
  
  v[v != 0]
}