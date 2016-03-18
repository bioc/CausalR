#' @title computes an approximate maximum D value or weight
#' @description
#' Computes an approximate maximum D value (or weight).
#' The calculation is  approximate since only the first valid D value that is round. This has been
#' done to speed up the overall algorithm - to get the exact answer use GetMaximumDValueFromTwoByTwoContingencyTable.
#' @param n_pp the count n++ from the prediction-observation contingency matrix
#' @param n_pm the count n+- from the prediction-observation contingency matrix
#' @param n_mp the count n-+ from the prediction-observation contingency matrix
#' @param n_mm the count n-- from the prediction-observation contingency matrix
#' @param predictionListStats a vector containing the values q+, q- and q0: the number of positive, negative, non-significant/contradictory predictions
#' @param experimentalDataStats a vector containing the values n+, n- and n0: the number of positive, negative, non-significant/contradictory observations 
#' @param logOfFactorialOfPredictionListStats a vector containing the log of the factorial value for each entry in predictionListStats
#' @param returnlog return the result as a log, default is FALSE
#' @return the maximum D value or weight



GetApproximateMaximumDValueFromTwoByTwoContingencyTable <- function(n_pp, n_pm, n_mp, n_mm, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, 
    returnlog = FALSE) {
    # A function that will compute an approximate maximum D value (or weight).  The reason it is described as approiximate is that this function returns
    # the first valid D value that is found. This has been done to speed up the overall algorithm - to get the exact answer use
    # GetMaximumDValueFromTwoByTwoContingencyTable.
    
    # Inputs: twoByTwoContingencyTable Approximate values of n++, n+-, n-+ and n--, these values are calculated to optimise the D-value (see page 6 of
    # Assessing statistical significance of causal graphs) predictionListStats A vector containing the values q+, q- and q0 (the number of
    # positive/negative/non-significant (or contradictory) predictions) experimentalDataStats A vector containing the values n+, n- and n0 (the number
    # of positive/negative/non-significant (or contradictory) transcripts in the results) logOfFactorialOfPredictionListStats
    
    threeByThreeContingencyTable <- PopulateTheThreeByThreeContingencyTable(n_pp, n_pm, n_mp, n_mm, predictionListStats, experimentalDataStats)
    
    # Some of the combinationations produce an invalid contingency table - we can check this by seeing if any of the values in the table are negative.
    if (min(threeByThreeContingencyTable) >= 0) {
        # To speed up calculation, we will only calculate the D value for the first valid combination.
        weight <- CalculateWeightGivenValuesInThreeByThreeContingencyTable(threeByThreeContingencyTable, logOfFactorialOfPredictionListStats, returnlog)
    } else {
        stop("Could not find a valid three by three contigency table given the approximate values in the two by two contingency table that will maximise the D-value.")
    }
    
    return(weight)
    
} 
