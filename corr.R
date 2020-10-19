#Function: takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 


#Essa traz os números certos mas no formato errado
corr <- function(directory, threshold = 0) {
  files <- dir(directory, full.names = TRUE)
  data.nobs <- data.frame()
  for (i in 1:length(files)) {
    data.nobs <- rbind(data.nobs, data.frame("file" = files[i], "nobs" = sum(complete.cases(read.csv(files[i])) == TRUE)))
    data.nobs.thr <- subset(data.nobs, nobs > threshold)
    files.thr <- data.nobs.thr$file
  }
  data.nitsul <- data.frame()
  for (i in 1:length(files.thr)) {
    data.nitsul <- rbind(data.nitsul, read.csv(files.thr[i]))
  }
  data.nitsul.nona <- na.omit(data.nitsul)
  data.nitsul.nona %>%
    group_by(ID) %>%
    summarise("cr" = cor(sulfate , nitrate , use = "complete.obs"))
}


#Essa traz o formato certo, mas só cola o último número
corr <- function(directory, threshold = 0) {
  files <- dir(directory, full.names = TRUE)
  data.nobs <- data.frame()
  for (i in 1:length(files)) {
    data.nobs <- rbind(data.nobs, data.frame("file" = files[i], "nobs" = sum(complete.cases(read.csv(files[i])) == TRUE)))
    data.nobs.thr <- subset(data.nobs, nobs > threshold)
    files.thr <- data.nobs.thr$file
  }
  corrs <- numeric(length(files.thr))
  for (i in length(files.thr)) {
    corrs[i] <- cor(read.csv(files.thr[i])$sulfate, read.csv(files.thr[i])$nitrate , use = "complete.obs")
  }
  corrs
}