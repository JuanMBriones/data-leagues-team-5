library(roxygen2)
library(devtools)

league <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

goals <- league[, c("FTHG", "FTAG")]

goalsMap <- table(goals)

relFreq <- prop.table(goalsMap)

colSum <- colSums(relFreq)

rowSum <- rowSums(relFreq)

#' Set a different data frame for calculate probability
#' @export 
#' @author Juan Ma
#' @name setDataSet
#' @title Set a data frame for analyzing 
#' 
#' @param a The new data frame
#' 
setDataFrame <- function(a) {
  league <- a
  
  goals <- league[, c("FTHG", "FTAG")]
  
  goalsMap <- table(goals)
  
  relFreq <- prop.table(goalsMap)
  
  colSum <- colSums(relFreq)
  
  rowSum <- rowSums(relFreq)
}

#' Calculate the probability for getting `a` goals away
#' @export 
#' @author Juan Ma
#' @name FTAGProb
#' @title Probability of a goals at home
#' 
#' @param a Index of goals away
#' @return The probability of get `a` goals away
#' @examples
#' FTAGProb(1)
#' > 1
#' FTAGProb(2)
#' > 3
FTAGProb <- function(a) {
  result <- colSum[a]
  return(result)
}

#' Calculate the probability for getting `a` goals at home
#' @export 
#' @author Juan Ma
#' @name FTHGProb
#' @title Probability of a goals at home
#' 
#' @param a Index of goals at home
#' @return The probability of get `a` goals at home
#' @examples
#' FTHGProb(1)
#' > 1
#' FTHGProb(2)
#' > 3
FTHGProb <- function(a) {
  result <- rowSum[a]
  return(result)
}


#' Calculate compose probability of the fact table
#' @export 
#' @author Juan Ma
#' @name ComposeProb
#' @title Probability of a and b goals
#' 
#' @param a Index of goals in home
#' @param b Index of gaols away
#' @return The value of the index of a and b of the matrix
#' @examples
#' composeProb(1, 1)
#' > 1
#' composeProb(2, 5)
#' > 3
composeProb <- function(a, b) {
  result <- relFreq[a+1, b+1] # por como trabaja R
  return(result)
}
#install.packages("roxygen2")
# devtools::load_all()
