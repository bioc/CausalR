#' @title   get scores weights matrix by the cubic algorithm
#' @description
#' Implements the cubic algorithm as described on pages 6 and 7 of Assessing statistical significance in causal graphs,
#' Chindelevitch et al. 2012
#' @param predictionListStats a vector containing the values q+, q- and q0 
#' @param experimentalDataStats a vector containing the values n+, n- and n0
#' @param epsilon the algorithms tolerance epsilon
#' @return a matrix containing the ternary dot product distribution


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.


GetScoresWeightsMatrixByCubicAlg <- function(predictionListStats, experimentalDataStats, epsilon) {
    
    
    logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
    
    DMax <- FindMaximumDValue(predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats)
    
    # Not all values of r_, r- c+, c- are possible for the contigency table. This step finds all that combinations that are valid. Each row of this
    # matrix will represent a family of contingency tables.
    matrixOfAllPossibleRowAndColumnSumValues <- GetRowAndColumnSumValues(predictionListStats, experimentalDataStats)
    
    # Ensure matrixOfAllPossibleRowAndColumnSumValues has 4 columns
    matrixOfAllPossibleRowAndColumnSumValues <- matrix(matrixOfAllPossibleRowAndColumnSumValues, ncol = 4)
    numOfCombinations <- nrow(matrixOfAllPossibleRowAndColumnSumValues)
    
    # For each combination of row and column sum values, the value of n++ is bounded by zero and q+ (predictionListStats[1]).  Hence the maximum number
    # of entries in distributionMatrix will be:
    maxNumberOfDistributionValues <- numOfCombinations * (predictionListStats[1] + 1)
    distributionMatrix <- matrix(0, maxNumberOfDistributionValues, 2)
    
    # A counter that will be used to keep track how many entries have been put into distributionMatrix
    i <- 1
    
    for (counter in 1:numOfCombinations) {
        rowAndColumnSumValues <- matrixOfAllPossibleRowAndColumnSumValues[counter, ]
        # To reduce the overall runtime of the algorithm, the approximate maxDValue for the family is calculated, rather than the actual.
        approxDFamilyMax <- GetMaxDValueForAFamily(rowAndColumnSumValues, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats)
        if (approxDFamilyMax > epsilon * DMax) {
            # Calculate the value of n++ that is used to calculate approxDFamilyMax r+
            r_p <- rowAndColumnSumValues[1]
            # c+
            c_p <- rowAndColumnSumValues[3]
            # t
            total <- sum(rowAndColumnSumValues[1:2])
            if (total > 0) {
                n_pp <- round(r_p * c_p/total)
            } else {
                n_pp <- 0
            }
            
            # Iterate over possible values of n++, starting from the one calculated above.  Since the values decrease monotonically either side of this value,
            # we start at this value and iterate in both directions until the D value is less than a given threshold. This will give us the non-neglible parts
            # of the distribution.
            
            # An upper bound for possible values of n++ is the minimum of r+ and c+
            for (u in n_pp:min(r_p, c_p)) {
                # The output to this function is a 1x4 vector containing the values of n++, n+-, n-+, n--
                contingencyTableValues <- PopulateTwoByTwoContingencyTable(rowAndColumnSumValues, u)
                # None of these values can be less than zero
                if (min(contingencyTableValues) >= 0) {
                    DValue <- GetWeightForNumbersOfCorrectandIncorrectPredictions(contingencyTableValues, predictionListStats, experimentalDataStats, 
                                                                                  logOfFactorialOfPredictionListStats)
                    if (DValue > epsilon * DMax) {
                        # Only need to compute the score if the D-value is non-neglible
                        score <- GetScoreForNumbersOfCorrectandIncorrectPredictions(contingencyTableValues)
                        distributionMatrix[i, ] <- c(score, DValue)
                        i <- i + 1
                    } else {
                        break
                    }
                }
            }
            
            # The loop below does exactly the same thing as the one above; it covers the cases that are not done in the loop above. A lower bound for possible
            # values of n++ is 0 To avoid n_pp become negative or double counting when n_pp = 0, we add the following if statement
            if (n_pp > 0) {
                
                for (u in (n_pp - 1):0) {
                    contingencyTableValues <- PopulateTwoByTwoContingencyTable(rowAndColumnSumValues, u)
                    if (min(contingencyTableValues) >= 0) {
                        DValue <- GetWeightForNumbersOfCorrectandIncorrectPredictions(contingencyTableValues, predictionListStats, experimentalDataStats, 
                                                                                      logOfFactorialOfPredictionListStats)
                        if (DValue > epsilon * DMax) {
                            score <- GetScoreForNumbersOfCorrectandIncorrectPredictions(contingencyTableValues)
                            distributionMatrix[i, ] <- c(score, DValue)
                            i <- i + 1
                        } else {
                            break
                        }
                    }
                }
            }
        }
    }
    
    # Trim this matrix so that rows are not filled in during the algorithm are removed, and ensure it has two columns
    distributionMatrix <- matrix(distributionMatrix[1:i - 1, ], ncol = 2)
    
    return(distributionMatrix)
} 
