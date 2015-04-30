#' @title score hypothesis
#' @description 
#' Score a single hypothesis, using the predictions from the network and the experimental data returning a vector of scoring statistics
#' @param matrixOfPredictions a matrix of predictions
#' @param matrixOfExperimentalData a matrix of experimentaldata
#' @return scoreBreakdown a vector giving, in order, the overall score, and the numbers of correct, incorrect and ambigous predictions
#' @export
#' @examples
#' predictions <- matrix(c(1,2,3,+1,0,-1),ncol=2)
#' experimentalData <- matrix(c(1,2,4,+1,+1,-1),ncol=2)
#' ScoreHypothesis(predictions,experimentalData)
#' CompareHypothesis(predictions,experimentalData)

ScoreHypothesis <- function(matrixOfPredictions, matrixOfExperimentalData){

  idx <- match(matrixOfExperimentalData[,1],matrixOfPredictions[,1])
  
  if (all(is.na(idx))){
    return(c(0,0,0,0))
  }else{
    # Use these to get the predicted values for the nodes in common
    predictedValues <- matrixOfPredictions[idx[!is.na(idx)],2]
    
    # The equivalent experimental values are the ones where idx is not NA
    experimentalValues <- matrixOfExperimentalData[!is.na(idx),2]
    
    individualScores <- predictedValues * experimentalValues
    
    overallScore <- sum(individualScores)
    correctPredictions <- sum(individualScores == 1)
    incorrectPredictions <- sum(individualScores == -1)
    ambiguousPredictions <- sum(predictedValues==0 & experimentalValues!=0)
    
    scoreBreakdown <- c(overallScore, correctPredictions, incorrectPredictions, ambiguousPredictions)
    
    return(scoreBreakdown)
  }
}
