#' @title get scores weight matrix
#' @description
#' Computes the score and weight for a network/set of experimental data based on the table containing possible values of n++, n+-, n-+ and n--.
#' @param  matrixOfPossibleValues values of n++, n+-, n-+ and n-- that need to be assessed 
#' @param  predictionDataStats a table of predicions
#' @param  experimentalDataStats a table of observed experimental data
#' @param  logOfFactorialOfPredictionListStats  a vector containing the log of the factorial value for
#' each entry in predictionListStats
#' @return a matrix containing scores and logs of the weights

GetScoresWeightsMatrix <- function(matrixOfPossibleValues, predictionDataStats, experimentalDataStats, logOfFactorialOfPredictionListStats) {
    
    matrixOfPossibleValues <- matrix(matrixOfPossibleValues, ncol = 4)
    
    numRows <- nrow(matrixOfPossibleValues)
    # A matrix to store the score and weight of each row in matrixOfPossibleValues
    scoresandLogOfWeightsMatrix <- matrix(0, numRows, 2)
    
    for (counter in 1:numRows) {
        # Compute the score
        score = GetScoreForNumbersOfCorrectandIncorrectPredictions(matrixOfPossibleValues[counter, ])
        scoresandLogOfWeightsMatrix[counter, 1] <- score
        # Compute D-value - all are returned as log of the actual value. This is because the actual value is usually too big and reported as Inf
        logOfWeight = GetWeightForNumbersOfCorrectandIncorrectPredictions(matrixOfPossibleValues[counter, 1], matrixOfPossibleValues[counter, 2], matrixOfPossibleValues[counter, 
            3], matrixOfPossibleValues[counter, 4], predictionDataStats, experimentalDataStats, logOfFactorialOfPredictionListStats, returnlog = TRUE)
        scoresandLogOfWeightsMatrix[counter, 2] <- logOfWeight
    }
    
    # Check total weight expectedWeight =
    # factorial(sum(predictionDataStats))/factorial(numUpregulatedGenes)*factorial(numDownregulatedGenes)*factorial(numNonResponsiveGenes)
    
    return(scoresandLogOfWeightsMatrix)
} 
