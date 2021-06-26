

#' Combine many files in a data frame with determined rows
#' @export 
#' @author Juan Ma
#' @name mergeFiles
#' @title Combine files 
#' 
#' @return The result data frame with the merged files
#' 
mergeFiles <- function() {
  library(dplyr)
  
  url1 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
  url2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
  url3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
  
  liga1718 <- read.csv(url1) # ("primeraLiga1718.csv")
  liga1819 <- read.csv(url2) # ("primeraLiga1819.csv")
  liga1920 <- read.csv(url3) # ("primeraLiga1920.csv")
  
  liga1718Filtrada <- select(liga1718, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  liga1819Filtrada <- select(liga1819, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  liga1920Filtrada <- select(liga1920, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  
  liga1718Filtrada <- mutate(liga1718Filtrada, Date = as.Date(Date, "%d/%m/%y")) # %d-%m-%y"))
  liga1819Filtrada <- mutate(liga1819Filtrada, Date = as.Date(Date, "%d/%m/%y")) # %d-%m-%y"))
  liga1920Filtrada <- mutate(liga1920Filtrada, Date = as.Date(Date, "%d/%m/%y")) # %d-%m-%y"))
  
  combinedLiga <- rbind(liga1718Filtrada, liga1819Filtrada, liga1920Filtrada)
  
  return(combinedLiga)
}
