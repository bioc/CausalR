# A test suite for the Quartic Algorithm
library(igraph)


test_CalculateSignificanceUsingQuarticAlgorithmContainingSingleNode <-  function(){
  # A number of very simple tests for networks with just 1,and 2 nodes
  # Tests for network with 1 node
  predictionsListStats <- c(1,0,0)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats, c(1,0,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats, c(0,1,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats, c(0,0,1)),1)
}

test_CalculateSignificanceUsingQuarticAlgorithmWithNetworkContainingTwoNodes <-  function(){
  # Tests involving a representative network with 2 nodes
  # Both nodes have positive predictions
  predictionsListStats2 <- c(2,0,0)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(2, predictionsListStats2, c(2,0,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-2, predictionsListStats2, c(0,2,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats2, c(0,0,2)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats2, c(1,1,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats2, c(1,0,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats2, c(0,1,1)),1)
  # Both nodes have negative predictions
  predictionsListStats3 <- c(0,2,0)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-2, predictionsListStats3, c(2,0,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(2, predictionsListStats3, c(0,2,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats3, c(0,0,2)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats3, c(1,1,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats3, c(1,0,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats3, c(0,1,1)),1)
  # One node has a positive prediction the other is negative (this is done without loss of 
  # generality)
  predictionsListStats4 <- c(1,1,0)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats4, c(2,0,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats4, c(0,2,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats4, c(0,0,2)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(2, predictionsListStats4, c(1,1,0)),0.5)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-2, predictionsListStats4, c(1,1,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats4, c(1,0,1)),0.5)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats4, c(1,0,1)), 1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats4, c(0,1,1)),0.5)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats4, c(0,1,1)),1)
}

test_CalculateSignificanceUsingQuarticAlgorithmWithNetworkContainingThreeNodes <-  function(){
  # Test involving a representative network with 3 nodes
  # All three nodes are positive
  predictionsListStats5 <- c(3,0,0)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(3, predictionsListStats5, c(3,0,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-3, predictionsListStats5, c(0,3,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats5, c(0,0,3)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats5, c(2,1,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(2, predictionsListStats5, c(2,0,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats5, c(1,2,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats5, c(1,1,1)), 1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats5, c(1,0,2)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-2, predictionsListStats5, c(0,2,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats5, c(0,1,2)),1)
  # 2 positive nodes and one negative
  predictionsListStats6 <- c(2,1,0)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats6, c(3,0,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats6, c(0,3,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats6, c(0,0,3)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(3, predictionsListStats6, c(2,1,0)),(1/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats6, c(2,1,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(2, predictionsListStats6, c(2,0,1)),(1/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats6, c(2,0,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats6, c(1,2,0)),(2/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-3, predictionsListStats6, c(1,2,0)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(2, predictionsListStats6, c(1,1,1)),(1/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats6, c(1,1,1)),(2/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-2, predictionsListStats6, c(1,1,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats6, c(1,0,2)),(2/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats6, c(1,0,2)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(0, predictionsListStats6, c(0,2,1)),(2/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-2, predictionsListStats6, c(0,2,1)),1)
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(1, predictionsListStats6, c(0,1,2)),(1/3))
  checkEquals(CausalR:::CalculateSignificanceUsingQuarticAlgorithm(-1, predictionsListStats6, c(0,1,2)),1)
}

test_CheckPossibleValuesAreValidPassesWhenGivenAValidCombination <- function(){
  checkTrue(CausalR:::CheckPossibleValuesAreValid(c(3,2,0), c(2,1,2), c(1,1,1,0)))
}

test_CheckPossibleValuesAreValidFaislWhenGivenAnInvalidCombination <- function(){
  # There are 10 conditions that are checked in the function - each one needs to be tested individually
  # The number of each condition is given in CheckPossibleValuesAreValid
  # Check condition 1 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(2,0,0), c(0,0,0), c(2,1,0,0))))
  # Check condition 2 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,2,0), c(0,0,0), c(0,0,2,1))))
  # Check condition 3 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,0,0), c(2,0,0), c(1,0,2,0))))
  # Check condition 4 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,0,0), c(0,2,0), c(0,1,0,2))))
  # Check condition 5 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(4,0,0), c(0,0,1), c(1,1,0,0))))
  # Check condition 6 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,4,0), c(0,0,1), c(0,0,1,1))))
  # Check condition 7 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,0,1), c(4,0,0), c(1,0,1,0))))
  # Check condition 8 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,0,1), c(0,4,0), c(0,1,0,1))))
  # Check condition 9 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(4,4,0), c(0,0,1), c(1,1,1,1))))
  # Check condition 9 fails appropriately 
  checkTrue(!(CausalR:::CheckPossibleValuesAreValid(c(0,0,1), c(4,4,0), c(1,1,1,1))))
}

test_ComputeFinalDistributionAggregatesWeightsCorrectly <-  function(){
  matrix1 <- matrix(c(12,4,-1,12,-1,0.3,0.1,0.1,0.2,0.3), ncol = 2)
  distributionMatrix <- CausalR:::ComputeFinalDistribution(matrix1)
  checkEquals(distributionMatrix [1,1],-1)
  checkEquals(distributionMatrix [2,1],4)
  checkEquals(distributionMatrix [3,1],12)
  checkEquals(distributionMatrix [1,2],0.4)
  checkEquals(distributionMatrix [2,2],0.1)
  checkEquals(distributionMatrix [3,2],0.5)
}

test_GetCombinationsOfCorrectandIncorrectPredictionsProducesCorrectOutput <- function(){
  predictionDataStats <- c(2,1,0)
  experimentalDataStats <- c(1,1,1)
  matrixWithNumbersOfCorrectandIncorrectPredictions <- CausalR:::GetCombinationsOfCorrectandIncorrectPredictions(predictionDataStats, experimentalDataStats)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[1,1], 0)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[1,2], 1)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[1,3], 1)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[1,4], 0)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[2,1], 1)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[2,2], 0)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[2,3], 0)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[2,4], 1)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[3,1], 1)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[3,2], 1)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[3,3], 0)
  checkEquals(matrixWithNumbersOfCorrectandIncorrectPredictions[3,4], 0)
}

test_GetScoresWeightsMatrixFillsTheTwoColumnsCorrectly <- function(){
  matrix1 <- matrix(c(0,1,1,1,0,1,1,0,0,0,1,0), ncol = 4)
  predictionDataStats <- c(2,1,0)
  experimentalDataStats <- c(1,1,1)
  logOfFactorialOfPredictionListStats <- c(log(factorial(2)),log(factorial(1)), log(factorial(0)))
  matrixWithScoresAndWeights <- CausalR:::GetScoresWeightsMatrix(matrix1, predictionDataStats, experimentalDataStats, logOfFactorialOfPredictionListStats) 
  checkEquals(dim(matrixWithScoresAndWeights)[1],3)
  checkEquals(dim(matrixWithScoresAndWeights)[2],2)
  checkEquals(matrixWithScoresAndWeights[1,1],-2)
  checkEquals(matrixWithScoresAndWeights[2,1],2)
  checkEquals(matrixWithScoresAndWeights[3,1],0)
  checkEquals(matrixWithScoresAndWeights[1,2],matrixWithScoresAndWeights[2,2])
  checkEquals(matrixWithScoresAndWeights[2,2],matrixWithScoresAndWeights[3,2])
}