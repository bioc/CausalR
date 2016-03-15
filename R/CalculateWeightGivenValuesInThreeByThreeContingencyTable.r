#' @title calculate weight given values in three-by-three contingency table
#' @description
#' Given the values in the three by three contingency table and the values of the number of positive/negative/non-significant predictions (q+, q-, q0) this function calculates the D-value (or weight).

#' @param threeByThreeContingencyTable a 3x3 contingency table
#' @param logOfFactorialOfPredictionListStats log of Factorial of prediction statistics
#' @param returnlog should the result be returned as a log value. Default is FALSE.

#' @return a D-value (or weight)

CalculateWeightGivenValuesInThreeByThreeContingencyTable <- function(threeByThreeContingencyTable, logOfFactorialOfPredictionListStats, returnlog = FALSE) {
    
    # The D value is defined as: (q+! / (n++! * n+-! * n+0!)) * (q-! / (n-+! * n--! * n-0!)) * (q0! / (n0+! * n0-! * n00!))
    
    if (returnlog) {
        return(sum(logOfFactorialOfPredictionListStats) - sum(lfactorial(threeByThreeContingencyTable)))
    } else {
        return(exp(sum(logOfFactorialOfPredictionListStats) - sum(lfactorial(threeByThreeContingencyTable))))
    }
} 
