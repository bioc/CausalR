# A test function for functions in Significance Calculator

library(igraph)


test_GetNumberOfPositiveAndNegativeEntries <- function(){
  dataList <- matrix(c(1,2,3,4,5,6,7, -1, 1, 1, 0, -1, 1, +1), ncol = 2)
  dataStats <- CausalR:::GetNumberOfPositiveAndNegativeEntries(dataList)
  checkEquals(dataStats[1], 4)
  checkEquals(dataStats[2], 2)
}

test_AnalysePredictionsList <- function(){
  predictionData <- matrix(c(1,2,3,4,5,6,7, -1, 1, 1, 0, -1, -1, +1), ncol = 2)
  predictionDataStats <- CausalR:::AnalysePredictionsList(predictionData,length(predictionData[,1]))
  checkEquals(predictionDataStats[1], 3)
  checkEquals(predictionDataStats[2], 3)
  checkEquals(predictionDataStats[3], 1)
}

test_GetScoreForNumbersOfCorrectandIncorrectPredictionsProducesCorrectOutput <- function(){
  checkEquals(CausalR:::GetScoreForNumbersOfCorrectandIncorrectPredictions(c(5,2,1,2)),4)
  checkEquals(CausalR:::GetScoreForNumbersOfCorrectandIncorrectPredictions(c(5,7,1,2)),-1) 
}

test_GetWeightForNumbersOfCorrectandIncorrectPredictionsProducesCorrectOutputWhenReturnLogIsFalse <- function(){
  logOfFactorialOfPredictionListStats <- c(log(factorial(3)),log(factorial(2)),log(factorial(0))) 
  checkEquals(CausalR:::GetWeightForNumbersOfCorrectandIncorrectPredictions(1,0,1,1, c(3,2,0), c(2,1,2),logOfFactorialOfPredictionListStats, returnlog=FALSE),6)
  checkEquals(CausalR:::GetWeightForNumbersOfCorrectandIncorrectPredictions(2,1,0,0, c(3,2,0), c(2,1,2),logOfFactorialOfPredictionListStats, returnlog=FALSE),3)
}

test_GetWeightForNumbersOfCorrectandIncorrectPredictionsProducesCorrectOutputWhenReturnLogIsTrue <- function(){
  logOfFactorialOfPredictionListStats <- c(log(factorial(3)),log(factorial(2)),log(factorial(0))) 
  checkEquals(CausalR:::GetWeightForNumbersOfCorrectandIncorrectPredictions(1,0,1,1, c(3,2,0), c(2,1,2),logOfFactorialOfPredictionListStats, returnlog=TRUE),log(6))
  checkEquals(CausalR:::GetWeightForNumbersOfCorrectandIncorrectPredictions(2,1,0,0, c(3,2,0), c(2,1,2),logOfFactorialOfPredictionListStats, returnlog=TRUE),log(3))
}

test_PopulateTheThreeByThreeContingencyTablePopulatesTableCorrectly <- function(){
  # Test a symmetric case
  predictionDataStats <- c(2,3,2)
  experimentalDataStats <- c(2,3,2)
  threeByThreeTable <- CausalR:::PopulateTheThreeByThreeContingencyTable(1,1,1,1, predictionDataStats, experimentalDataStats)
  checkEquals(length(threeByThreeTable), 9)
  checkEquals(threeByThreeTable[1], 1)
  checkEquals(threeByThreeTable[2], 1)
  checkEquals(threeByThreeTable[3], 0)
  checkEquals(threeByThreeTable[4], 1)
  checkEquals(threeByThreeTable[5], 1)
  checkEquals(threeByThreeTable[6], 1)
  checkEquals(threeByThreeTable[7], 0)
  checkEquals(threeByThreeTable[8], 1)
  checkEquals(threeByThreeTable[9], 1)
  predictionDataStats2 <- c(4,3,2)
  experimentalDataStats2 <- c(2,3,4)
  threeByThreeTable2 <- CausalR:::PopulateTheThreeByThreeContingencyTable(1,1,1,1, predictionDataStats2, experimentalDataStats2)
  checkEquals(length(threeByThreeTable), 9)
  checkEquals(threeByThreeTable2[1], 1)
  checkEquals(threeByThreeTable2[2], 1)
  checkEquals(threeByThreeTable2[3], 2)
  checkEquals(threeByThreeTable2[4], 1)
  checkEquals(threeByThreeTable2[5], 1)
  checkEquals(threeByThreeTable2[6], 1)
  checkEquals(threeByThreeTable2[7], 0)
  checkEquals(threeByThreeTable2[8], 1)
  checkEquals(threeByThreeTable2[9], 1)
}

test_ComputePValueFromDistributionTableFromDistributionTableWorksGivenADistributionTable <- function(){
  matrix1 <- matrix(c(1,2,3,0.5,0.4,0.1), ncol=2)
  totalWeights <- sum(matrix1[,2])
  checkEquals(CausalR:::ComputePValueFromDistributionTable(1, matrix1, totalWeights), 1)
  checkEquals(CausalR:::ComputePValueFromDistributionTable(2, matrix1, totalWeights), 0.5)
  checkEquals(CausalR:::ComputePValueFromDistributionTable(3, matrix1, totalWeights), 0.1)
  checkEquals(CausalR:::ComputePValueFromDistributionTable(4, matrix1, totalWeights), 0)
}

test_ComputePValueFromDistributionTableWorksGivenUnnormalisedDistributionTable <- function(){
  matrix1 <- matrix(c(1,2,3,5,4,1), ncol=2)
  totalWeights <- sum(matrix1[,2])
  checkEquals(CausalR:::ComputePValueFromDistributionTable(1, matrix1, totalWeights), 1)
  checkEquals(CausalR:::ComputePValueFromDistributionTable(2, matrix1, totalWeights), 0.5)
  checkEquals(CausalR:::ComputePValueFromDistributionTable(3, matrix1, totalWeights), 0.1)
  checkEquals(CausalR:::ComputePValueFromDistributionTable(4, matrix1, totalWeights), 0)
}

test_CalculateSignificanceCallsCalculateSignificanceUsingCubicAlgorithm  <- function(){
  # Check that CalculateSignificance calls CalculateSignificanceUsingCubicAlgorithm correctly
  pValue_direct <- CalculateSignificanceUsingCubicAlgorithm(5, c(7,4,9), c(6,6,8), 1e-5)
  pValue_indirect <- CausalR:::CalculateSignificance(5, c(7,4,9), c(6,6,8), 1e-5, TRUE)
  checkEquals(pValue_direct,pValue_indirect)
}

test_CalculateSignificanceCallsCalculateSignificanceUsingQuarticAlgorithm  <- function(){
  # Check that CalculateSignificance calls CalculateSignificanceUsingQuarticAlgorithm correctly
  pValue_direct <- CalculateSignificanceUsingQuarticAlgorithm(5, c(7,4,9), c(6,6,8))
  pValue_indirect <- CausalR:::CalculateSignificance(5, c(7,4,9), c(6,6,8), useCubicAlgorithm=FALSE)
  checkEquals(pValue_direct,pValue_indirect)
}
