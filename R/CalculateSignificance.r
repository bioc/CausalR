#' @title calculate overall significance p-value
#' @description
#' Calculates the p-value of a score given the hypothesis score and the 
#' distribution table, using either the quartic or the (faster) cubic algorithm

#' @export
#' @param hypothesisScore score for a particular hypothesis 
#' @param predictionListStats numbers of predicted up-regulated, 
#' predicted down-regulated and ambiguous predictions predicted by the algorithm 
#' @param experimentalResultStats numbers of up-regulated, 
#' down-regulated and not significantly changed transcripts in the 
#' experimental data 
#' @param epsilon threshold that is used when calculating the p-value 
#' using the cubic algorithm 
#' @param useCubicAlgorithm  use the cubic algorithm, defaults to TRUE
#' @param use1bAlgorithm  use the 1b version of the algorithm, defaults to TRUE
#' used to calculate the p-value
#' @return the resulting  p-value
#' @examples
#' CalculateSignificance(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingQuarticAlgorithm(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=FALSE)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), 1e-5)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), epsilon=1e-5, useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingCubicAlgorithm(5, c(7,4,19), c(6,6,18), 1e-5)



CalculateSignificance <- function(hypothesisScore, predictionListStats, experimentalResultStats, epsilon=1e-5, useCubicAlgorithm=TRUE, use1bAlgorithm=TRUE){
  
  if(useCubicAlgorithm){
    if (use1bAlgorithm){
      pValue <- CalculateSignificanceUsingCubicAlgorithm1b(hypothesisScore, predictionListStats, experimentalResultStats, epsilon)
    }    
    else{
      pValue <- CalculateSignificanceUsingCubicAlgorithm(hypothesisScore, predictionListStats, experimentalResultStats, epsilon)
    }
  }
  else{
    pValue <- CalculateSignificanceUsingQuarticAlgorithm(hypothesisScore, predictionListStats, experimentalResultStats)
  }
  return(pValue)
}