#' @title compute a p-value from the distribution table
#' @description
#' Computes the p-value of the score of an hypothesis, based on a distribution table

#' @param scoreOfHypothesis a score of hypothesis
#' @param distributionMatrix a distribution table presented as a matrix
#' @param totalWeights a matrix of total weights
#' @return a p-value


ComputePValueFromDistributionTable <- function(scoreOfHypothesis, distributionMatrix, totalWeights) {
    
    
    # Ensure distribution matrix has two columns
    distributionMatrix <- matrix(distributionMatrix, ncol = 2)
    
    pValue <- sum(distributionMatrix[distributionMatrix[, 1] >= scoreOfHypothesis, 2])/totalWeights
    
    return(pValue)
} 
