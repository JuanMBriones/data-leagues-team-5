
#' Plot Heat Map based on historic data
#' @export 
#' @author Juan Ma
#' @name plotHeatMap
#' @title Plot Heat Map 
#' 
plotHeatMap <- function() {
  library(dplyr)
  library(DataLeagues)
  library(ggplot2)
  library(plotly)
  
  setDataFrame(mergeFiles())
  
  homeGoals <- FTHGProb(c(0:3))
  awayGoals <- FTAGProb(c(0:3))
  homeAwayGoals <- composeProb(c(0:3), c(0:3))
  
  homeAwayGoalsDF <- as.data.frame(homeAwayGoals)
  
  myHeatmap <- ggplot(homeAwayGoalsDF, aes(x=FTHG, y=FTAG, fill=Freq)) +
    geom_tile() +
    xlab('Goles en casa') +
    ylab('Goles en Fuera') 
  
  ggplotly(myHeatmap)
  
}
