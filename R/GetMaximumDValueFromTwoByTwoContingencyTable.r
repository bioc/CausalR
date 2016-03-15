#' @title get maximum D value from two-by-two contingency table
#' @description
#' Computes the maximum D value (or weight) given approximate values of n++, n+-, n-+ and n--.
#' These values are approximate and in general are non-integer values; they are found by using an 
#' approximation detailed in the paper Assessing statistical significance in causal graphs on page 6 i.e. n_ab is approximately
#' equal to q_a*n_b/t where a and b are either +, - or 0. The value is an approximation since the direction in which the 
#' number should be rounded is not clear and hence this function runs through all possible combinations of rounding before
#' concluding the maximum D-value.


#' @param twoByTwoContingencyTable approximate values of n++, n+-, n-+ and n--, these values arecalculated to optimise the D-value
#' @param predictionListStats a vector containing the values q+, q- and q0 the number of positive/negative/non-significant (or contradictory) predictions)
#' @param experimentalDataStats a vector containing the values n+, n- and n0 (the number of positive/negative/non-significant (or contradictory) transcripts in the results)
#' @param logOfFactorialOfPredictionListStats a vector containing the log of the factorial value for each entry in predictionListStats
#' @param returnlog whether or not he value should be returned as a log (TRUE) or not (FALSE)
#' @return the maximal D-value


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.


GetMaximumDValueFromTwoByTwoContingencyTable <- function(twoByTwoContingencyTable, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, 
    returnlog = FALSE) {
    # A function that will compute the maximum D value (or weight) given approximate values of n++, n+-, n-+ and n--.  The reason these values are
    # described as approximate is that they are non-integer values; they are found by using an approximation detailed in the paper Assessing statistical
    # significance in causal graphs on page 6 i.e. n_ab is approximately equal to q_a*n_b/t where a and b are either +, - or 0. The value is an
    # approximation since the direction in which the number should be rounded is not clear and hence this function runs through all possible
    # combinations of rounding before concluding the maximum D-value.
    
    
    # We need to find all the combinations (maximum of 16) when applying the rounding to the values in twoByTwoContingencyTable.  The exception is if
    # all four values are integers.  combinationsOfRounding will be a nx4 matrix containing the possible values of n_pp, n_pm, n_mp, n_mm
    if (mean(twoByTwoContingencyTable == round(twoByTwoContingencyTable)) == 1) {
        # All four values are integers.
        combinationsOfRounding <- matrix(twoByTwoContingencyTable, ncol = 4)
    } else {
        combinationsOfRounding <- GetAllPossibleRoundingCombinations(twoByTwoContingencyTable)
    }
    
    maximumDValue <- 0
    
    for (i in 1:nrow(combinationsOfRounding)) {
        numbersOfCorrectandIncorrectPredictions <- combinationsOfRounding[i, ]
        threeByThreeContingencyTable <- PopulateTheThreeByThreeContingencyTable(numbersOfCorrectandIncorrectPredictions[1], numbersOfCorrectandIncorrectPredictions[2], 
            numbersOfCorrectandIncorrectPredictions[3], numbersOfCorrectandIncorrectPredictions[4], predictionListStats, experimentalDataStats)
        # Some of the combinationations produce an invalid contingency table - this we can check by seeing if any of the values in the table are negative.
        if ((min(threeByThreeContingencyTable) >= 0)) {
            weight <- CalculateWeightGivenValuesInThreeByThreeContingencyTable(threeByThreeContingencyTable, logOfFactorialOfPredictionListStats, returnlog)
            maximumDValue <- max(maximumDValue, weight)
        }
    }
    
    return(maximumDValue)
} 
