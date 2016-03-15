#' @title compute final distribution
#' @description
#' Computes a final reference distribution of the score used to compute the final p-value. 

#' @param resultsMatrix a matrix containing the scores and weights from which the distribution is to be calculated
#' @return distributionMatrix  a matrix containing the reference distribution for the score 


# *** Summary *** Description: Takes the matrix which contains the scores and weights for the different n++, n+- etc.  values and combines those
# with the same score to get a matrix which represents the distribution of what scores the hypothesis can have under certain restrictions

# resultsTable contains the scores and weights. We are no longer interest in the individual values of n++, n+- etc., we just want to find the
# distribution of scores.

# When resultsMatrix is only one row (two values), the max and min functions below don't work because the array has been changed to a vector. To put
# it back into the form of a matrix the following if command is used

ComputeFinalDistribution <- function(resultsMatrix) {
    if (length(resultsMatrix) == 2) {
        resultsMatrix <- t(matrix(resultsMatrix, nrow = 2))
    }
    
    maxScore = max(resultsMatrix[, 1])
    minScore = min(resultsMatrix[, 1])
    
    # Pre-allocate the size of the storage array
    distributionMatrix <- matrix(0, (maxScore - minScore + 1), 2)
    numRows <- nrow(resultsMatrix)
    
    counter <- 1
    for (score in minScore:maxScore) {
        probability <- 0
        for (i in 1:numRows) {
            if (score == resultsMatrix[i, 1]) {
                probability <- probability + resultsMatrix[i, 2]
            }
        }
        if (probability > 0) {
            distributionMatrix[counter, ] <- c(score, probability)
            counter <- counter + 1
        }
    }
    
    # Remove the rows that were not populated in the for loops above
    distributionMatrix <- distributionMatrix[1:counter - 1, ]
    
    return(distributionMatrix)
} 
