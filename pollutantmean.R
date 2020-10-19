
#Function: calculates the mean of a pollutant (sulfate or nitrate) across a specified list of pollution monitors; each pollution monitor data is stored in a different file

pollutantmean <- function(directory, pollutant, id = 1:332) {
  files.total <- dir(directory, full.names = TRUE)            #stores a list of all files in the wanted directory
  id <- id                                                    #stores a vector of length equal to number of wanted files (monitors to be read)
  files.want <- files.total[id[1]:id[length(id)]]             #subsets the list of all files to a list of wanted files
  nfiles <- length(files.want)                                #stores a vector that tells how many fils to read
  data.bind <- data.frame()                                   #generates an empty data frame
  for (i in 1:nfiles) {                                       
    data.bind <- rbind(data.bind, read.csv(files.want[i]))    #will r-bind all of the wanted files and stpre them  in the previously empty data frame
  }
  mean(data.bind[,pollutant], na.rm = TRUE)                   #reports the mean of the wanted pollutant in the created data frame
}