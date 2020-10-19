
#Function that adds up two numbers x and y
add2 <- function(x,y) {
  x + y
}

#Function that returns all numbers above 10 from a vector
above10 <- function(x){
  use <- x>10
  x[use]
}

#Function that returns all numbers above a specified value from a vector
above <- function(x,n){
  use <- x > n
  x[use]
}

#Function that returns all numbers above a specified value, previsouly standardized to "10", from a vector
above <- function(x, n =10) {
  use <- x > n
  x[use]
}

#Function that returns the mean of each column in a df/matrix with option to remove NAs, standardized to "yes, remove"
mean_col <- function(x , removeNA = TRUE) {
  num_col <- ncol(x)
  means <- numeric(num_col)
  for (i in 1:num_col) {
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}
