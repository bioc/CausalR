#' @title returns the score for a given number of correct and incorrect predictions
#' @description
#' Returns the score based on the values of n++, n+-, n-+ and n--
#' @param matrixRow a row af a matrix of correct and incorrect prediction scores
#' @return the corresponding score for the given row


GetScoreForNumbersOfCorrectandIncorrectPredictions <- function(matrixRow){

numCorrectPositivePredictions <- matrixRow[1]
numIncorrectPositivePredictions <- matrixRow[2]
numIncorrectNegativePredictions <- matrixRow[3]
numCorrectNegativePredictions <- matrixRow[4] 

score <- (numCorrectPositivePredictions + numCorrectNegativePredictions) - 
		  (numIncorrectPositivePredictions + numIncorrectNegativePredictions)

return(score)
}