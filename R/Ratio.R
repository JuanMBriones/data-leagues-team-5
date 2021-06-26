library(roxygen2)
library(devtools)


#' Get the probability ratio between the joint probability and the product of 
#' each marginal probability.
#' @export 
#' @author Emiliano
#' @name probRatio
#' @title Get the probability ratio
#' 
#' @param md merged dataframe
#' @return Returns a dataframe with home team goals, foreign team goals and its probability ratio.
#' 
probRatio <- function(md) {
  library(tensor)
  t1 <- prop.table(table(md$FTHG))
  t2 <- prop.table(table(md$FTAG))
  t3 <- prop.table(table(md$FTHG, md$FTAG))
  ext_prod <- as.data.frame(tensor::tensor(t1, t2))
  ext_prod_inv <- apply(ext_prod, c(1, 2), '^', -1)
  res <- as.data.frame(ext_prod_inv * t3)
  res <- rename(res, c(Gols_casa=Var1, Gols_vis=Var2, cociente=Freq))
  return(res)
}