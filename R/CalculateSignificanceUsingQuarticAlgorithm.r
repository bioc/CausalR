#' @title calculate significance using the quartic algorithm
#' @description
#' Computes the significance of a given hypothesis. For a detailed description of the algorithm
#' see Causal reasoning on biological networks: interpreting transcriptional changes - Chindelevitch et al., section 2.
#' from which the methods and notation is taken.


#' @export
#' @param hypothesisScore  the score for which a  p-value is required
#' @param predictionListStats  a vector containing the values q+, q- and q0 (the number of positive/negative/non-significant  or contradictory) predictions)
#' @param experimentalDataStats a vector containing the values n+, n- and n0 (the number of positive/negative/non-significant (or contradictory) transcripts in the results) (or contradictory) transcripts in the results)
#' @return  the corresponding p-value
#' @examples
#' CalculateSignificance(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingQuarticAlgorithm(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=FALSE)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), 1e-5)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), epsilon=1e-5, useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingCubicAlgorithm(5, c(7,4,19), c(6,6,18), 1e-5)

#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.

CalculateSignificanceUsingQuarticAlgorithm <- function(hypothesisScore, predictionListStats, experimentalDataStats){
  
  # Each row of the matrix below will contain the four values: numCorrectPositivePredictions (n++),
  # numIncorrectPositivePredictions (n+-), numCorrectNegativePredictions n(-+) and numIncorrectNegativePredictions (n--)
  matrixOfIncorrectAndCorrectPredictionNumbers <- GetCombinationsOfCorrectandIncorrectPredictions(predictionListStats, experimentalDataStats)
  
  # Calculate the log of the factorial for all values in predictionListStats, this is to 
  # reduce the number of computations done later on. 
  logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
  
  scoresAndLogOfWeightsMatrix <- GetScoresWeightsMatrix(matrixOfIncorrectAndCorrectPredictionNumbers, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats)
  
  # Shift the logOfWeight values by subtracting the minimum away from all of them
  scoresAndLogOfWeightsMatrix[,2] <- scoresAndLogOfWeightsMatrix[,2] - min(scoresAndLogOfWeightsMatrix[,2]) 
  
  # Replace the logOfWeight values with Weights by taking the exponential and store in a matrix. By subtracting a factor from all the terms in 
  # the previous step we have divided all the weight values by a certain factor. This means we will get the correct answer
  # when computing the ratio of those which have a score greater than the hypothesis score (i.e. the p-value)
  scoresWeightsMatrix <- scoresAndLogOfWeightsMatrix
  scoresWeightsMatrix[,2] <- exp(scoresWeightsMatrix[,2]) 
  
  totalWeights <- sum(scoresWeightsMatrix[,2])
  
  pValue <- ComputePValueFromDistributionTable(hypothesisScore, scoresWeightsMatrix, totalWeights)
  
  return(pValue)
}