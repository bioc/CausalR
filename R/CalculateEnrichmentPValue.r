#' @title 
#' calculates an enrichment p-value
#' @description
#' Calculate a enrichment p-value for a given hypothesis by comparing the corresponding predicted and observed gene changes

#' @export
#' @param  predictions  predictions of changes from the CCG for a particular hypothesis
#' @param  results  gene changes observed in the experimental data
#' @return  an enrichment p-value
#' @examples
#' predictions <- matrix(c(1,2,3,1,1,-1), ncol = 2)
#' results<- matrix(c(1,2,3,4,1,1,-1,1), ncol = 2)
#' CalculateEnrichmentPValue(predictions, results)

CalculateEnrichmentPValue <- function(predictions, results){
 
  setOfSignificantPredictions <- GetSetOfSignificantPredictions(predictions)
  setOfDifferentiallyExpressedGenes <- GetSetOfDifferentiallyExpressedGenes(results)
  setOfNonDifferentiallyExpressedGenes <- setdiff(results[,1], setOfDifferentiallyExpressedGenes)
  
  # n_pp + n_pm + n_mp + n_mm
  numSignificantPredictionsThatAreResponsive <- length(intersect(setOfSignificantPredictions,setOfDifferentiallyExpressedGenes))
  # n+0 + n-0
  numSignificantPredictionsThatAreUnresponsive <- length(intersect(setOfSignificantPredictions,setOfNonDifferentiallyExpressedGenes))
  # n0+ + n0- 
  numZeroPredictionsThatAreResponsive <- length(setOfDifferentiallyExpressedGenes) - numSignificantPredictionsThatAreResponsive
  # n00
  numZeroPredictionsThatAreUnreponsive <- length(setOfNonDifferentiallyExpressedGenes) - numSignificantPredictionsThatAreUnresponsive
  
  contingencyTable <- matrix(c(numSignificantPredictionsThatAreResponsive,numSignificantPredictionsThatAreUnresponsive,numZeroPredictionsThatAreResponsive, numZeroPredictionsThatAreUnreponsive), nrow = 2)
  enrichmentPValue <- fisher.test(contingencyTable, alternative = "greater")[[1]]
  
  return(enrichmentPValue)
}