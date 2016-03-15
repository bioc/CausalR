#' @title 
#' analyse predictions list
#' @description
#' Taking the list of predictions from a particular hypothesis, counts the number of
#' positive and negative predictions in the list and the number of 0's (from numPredictions).


#' @param predictionsList list of predictions
#' @param numPredictions  number of predictions
#' @return prediction statistics
#' @export
#' @concept CausalR
#' @examples
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' ccg <- CreateCCG(network)
#' predictions <- MakePredictions('NodeA', +1, ccg, 2)
#' AnalysePredictionsList(predictions,8)

AnalysePredictionsList <- function(predictionsList, numPredictions) {
    
    predictionsListStats <- GetNumberOfPositiveAndNegativeEntries(predictionsList)
    predictionsListStats[3] <- numPredictions - (predictionsListStats[1] + predictionsListStats[2])
    
    return(predictionsListStats)
} 
