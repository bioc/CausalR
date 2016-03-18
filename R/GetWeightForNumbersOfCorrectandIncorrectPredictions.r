#' @title get weight for numbers of correct and incorrect predictions
#' @description 
#' Gets the weight based on the values of n++, n+-, n-+ and n--.
#' @param n_pp the contingency table entry n++
#' @param n_pm the contingency table entry n+-
#' @param n_mp the contingency table entry n-+
#' @param n_mm the contingency table entry n--
#' @param predictionDataStats prediction data statistics
#' @param experimentalDataStats  experimental data statistics
#' @param logOfFactorialOfPredictionListStats  log of factorial of prediction list stats
#' @param returnlog true if the result should be returned as a log
#' @return none

GetWeightForNumbersOfCorrectandIncorrectPredictions <- function(n_pp, n_pm, n_mp, n_mm, predictionDataStats, experimentalDataStats, logOfFactorialOfPredictionListStats, 
    returnlog = FALSE) {
    
    # contingencyTableValues contains the values of n++, n+-, n-+ and n-- (in that order) - the top left of the contingency table
    threeByThreeContingencyTable <- PopulateTheThreeByThreeContingencyTable(n_pp, n_pm, n_mp, n_mm, predictionDataStats, experimentalDataStats)
    
    if (returnlog) {
        return(sum(logOfFactorialOfPredictionListStats) - sum(lfactorial(threeByThreeContingencyTable)))
    } else {
        return(exp(sum(logOfFactorialOfPredictionListStats) - sum(lfactorial(threeByThreeContingencyTable))))
    }
} 
