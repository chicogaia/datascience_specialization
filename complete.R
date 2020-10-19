#Function: reads a directory full of files and reports the number of completely observed cases in each data file.

complete <- function(directory, id = 1:332){
  files.total <- dir(directory, full.names = TRUE)
  id <- id
  files.want <- files.total[id]
  nfiles <- length(files.want)
  data.nobs <- data.frame()
  for (i in 1:nfiles) {
    data.nobs <- rbind(data.nobs, data.frame("id" = id[i] , "nobs" = sum(complete.cases(read.csv(files.want[i])) == TRUE)))
  }
  data.nobs
}