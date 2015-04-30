# A test suite for the functions implemented in the cubic algorithm


library(igraph)


test_FindMaximumDValueProducesTheCorrectValue <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(1,1,1)
  logOfFactorialOfPredictionListStats <- c(log(factorial(2)),log(factorial(1)), log(factorial(0)))
  checkEquals(CausalR:::FindMaximumDValue(predictionListStats, experimentalResultStats,logOfFactorialOfPredictionListStats), 2)
  experimentalResultStats2 <- c(2,1,0)
  checkEquals(CausalR:::FindMaximumDValue(predictionListStats, experimentalResultStats2, logOfFactorialOfPredictionListStats), 2)
}

test_FindApproximateValuesThatWillMaximiseDValueCalculatesOutputValuesCorrectly <- function(){
  predictionListStats <- c(7,3,1)
  experimentalDataStats <- c(4,5,2)
  values <- CausalR:::FindApproximateValuesThatWillMaximiseDValue(predictionListStats, experimentalDataStats)
  checkEquals(length(values),4)
  checkEquals(values[1],(28/11))
  checkEquals(values[2],(35/11))
  checkEquals(values[3],(12/11))
  checkEquals(values[4],(15/11))
}

test_GetMaximumDValueFromTwoByTwoContingencyTableProducesTheCorrectDValue <- function(){
  twoByTwoContingencyTable <- c(1,2,0,1) 
  predictionListStats <- c(4,3,2)
  experimentalDataStats <- c(2,3,4)
  logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
  checkEquals(CausalR:::GetMaximumDValueFromTwoByTwoContingencyTable(twoByTwoContingencyTable, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats),72)
}

test_GetApproximateMaximumDValueFromTwoByTwoContingencyTableProducesTheCorrectDValue <- function(){
  n_pp <- 1
  n_pm <- 2
  n_mp <- 0
  n_mm <- 1  
  predictionListStats <- c(4,3,2)
  experimentalDataStats <- c(2,3,4)
  logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
  checkEquals(CausalR:::GetApproximateMaximumDValueFromTwoByTwoContingencyTable(n_pp, n_pm, n_mp, n_mm, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats),72)
}

test_GetAllPossibleRoundingCombinationsWithFourNonIntegerValues <- function(){
  vect <- c(4.2, 5.7, 2.3, 0.7)
  roundingCombinations <- CausalR:::GetAllPossibleRoundingCombinations(vect)
  checkEquals(nrow(roundingCombinations), 16)
  checkEquals(ncol(roundingCombinations), 4)
  checkEquals(roundingCombinations[1,], c(4,5,2,0))
  checkEquals(roundingCombinations[2,], c(4,5,2,1))
  checkEquals(roundingCombinations[3,], c(4,5,3,0))
  checkEquals(roundingCombinations[4,], c(4,5,3,1))
  checkEquals(roundingCombinations[5,], c(4,6,2,0))
  checkEquals(roundingCombinations[6,], c(4,6,2,1))
  checkEquals(roundingCombinations[7,], c(4,6,3,0))
  checkEquals(roundingCombinations[8,], c(4,6,3,1))
  checkEquals(roundingCombinations[9,], c(5,5,2,0))
  checkEquals(roundingCombinations[10,], c(5,5,2,1))
  checkEquals(roundingCombinations[11,], c(5,5,3,0))
  checkEquals(roundingCombinations[12,], c(5,5,3,1))
  checkEquals(roundingCombinations[13,], c(5,6,2,0))
  checkEquals(roundingCombinations[14,], c(5,6,2,1))
  checkEquals(roundingCombinations[15,], c(5,6,3,0))
  checkEquals(roundingCombinations[16,], c(5,6,3,1))
}

test_GetAllPossibleRoundingCombinationsWithOneIntegerValue <- function(){
  vect <- c(4.2, 5.7, 2.3, 1)
  roundingCombinations <- CausalR:::GetAllPossibleRoundingCombinations(vect)
  checkEquals(nrow(roundingCombinations), 8)
  checkEquals(ncol(roundingCombinations), 4)
  checkEquals(roundingCombinations[1,], c(4,5,2,1))
  checkEquals(roundingCombinations[2,], c(4,5,3,1))
  checkEquals(roundingCombinations[3,], c(4,6,2,1))
  checkEquals(roundingCombinations[4,], c(4,6,3,1))
  checkEquals(roundingCombinations[5,], c(5,5,2,1))
  checkEquals(roundingCombinations[6,], c(5,5,3,1))
  checkEquals(roundingCombinations[7,], c(5,6,2,1))
  checkEquals(roundingCombinations[8,], c(5,6,3,1))
}

test_GetMaxDValueForAFamilyProducesTheCorrectDValue <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(1,1,1)
  logOfFactorialOfPredictionListStats <- c(log(factorial(2)),log(factorial(1)), log(factorial(0)))
  checkEquals(CausalR:::GetMaxDValueForAFamily(2, 0, 1, predictionListStats, experimentalResultStats, logOfFactorialOfPredictionListStats), 2)
}

test_GetRowAndColumnSumValuesOutputsTheCorrectCombinations <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(1,1,1)
  possibleRowAndColumnSumValues <- CausalR:::GetRowAndColumnSumValues(predictionListStats, experimentalResultStats)
  checkEquals(nrow(possibleRowAndColumnSumValues), 2)
  checkEquals(ncol(possibleRowAndColumnSumValues), 4)
  checkEquals(possibleRowAndColumnSumValues[1,1], 1)
  checkEquals(possibleRowAndColumnSumValues[1,2], 1)
  checkEquals(possibleRowAndColumnSumValues[1,3], 1)
  checkEquals(possibleRowAndColumnSumValues[1,4], 1)
  checkEquals(possibleRowAndColumnSumValues[2,1], 2)
  checkEquals(possibleRowAndColumnSumValues[2,2], 0)
  checkEquals(possibleRowAndColumnSumValues[2,3], 1)
  checkEquals(possibleRowAndColumnSumValues[2,4], 1)
}


test_CheckRowAndColumnSumValuesAreValidReturnsTrueForValidInput <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(1,1,1)
  rowAndColumnSumValues <- c(2,0,1,1)
  checkTrue(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues, predictionListStats, experimentalResultStats))
}

test_CheckRowAndColumnSumValuesAreValidReturnsFalseForInvalidInput <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(1,1,1)
  rowAndColumnSumValues1 <- c(0,0,1,0)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues1, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues2 <- c(0,0,0,0)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues2, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues3 <- c(1,0,0,1)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues3, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues4 <- c(2,0,0,2)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues4, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues5 <- c(0,1,0,1)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues5, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues6 <- c(1,1,0,2)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues6, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues7 <- c(2,1,0,3)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues7, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues8 <- c(0,0,1,-1)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues8, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues9 <- c(1,0,1,0)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues9, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues10 <- c(0,1,1,0)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues10, predictionListStats, experimentalResultStats)))
  rowAndColumnSumValues10 <- c(2,1,1,-2)
  checkTrue(!(CausalR:::CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues10, predictionListStats, experimentalResultStats)))
}

test_GetWeightsAboveHypothesisScoreAndTotalWeightsProducesCorrectDistributionTable <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(2,1,0)
  epsilon = 1e-5
  logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
  hypothesisScore <- 1
  logDMax <- log(2)
  logepsDMax <- log(epsilon) + logDMax
  r_p <- 2
  r_m <- 1
  c_p <- 2
  # A 2x1 array containing the weight above the hypothesis score and the total weight
  weightAboveHypScoreAndTotalWeight <- CausalR:::GetWeightsAboveHypothesisScoreAndTotalWeights(r_p, r_m, c_p, predictionListStats, experimentalResultStats, logOfFactorialOfPredictionListStats, hypothesisScore, logepsDMax, logDMax)
  checkEquals(weightAboveHypScoreAndTotalWeight[1], 0.5)
  checkEquals(weightAboveHypScoreAndTotalWeight[2], 1.5)
}

test_GetWeightsAboveHypothesisScoreAndTotalWeightsDoesntDoubleCountWhenNPlusPlusIsZero <- function(){
  predictionListStats <- c(0,1,1)
  experimentalDataStats <- c(1,1,0)
  epsilon = 1e-5
  r_p <- 0
  r_m <- 1
  c_p <- 1
  logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
  hypothesisScore <- 0
  logDMax <- 0
  logepsDMax <- log(epsilon) + logDMax
  weightAboveHypScoreAndTotalWeight <- CausalR:::GetWeightsAboveHypothesisScoreAndTotalWeights(r_p, r_m, c_p, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, hypothesisScore, logepsDMax, logDMax)
  # There's only 1 element in the contingency table, a wrongly-predicted hypothesis, hence the score is -1, 
  # so it should contribute to the denominator of the p-value but not the numerator.
  checkEquals(weightAboveHypScoreAndTotalWeight[1],0)
  checkEquals(weightAboveHypScoreAndTotalWeight[2], 1)
}

test_CalculateSignificanceUsingCubicAlgorithmProducesExactAnswerWithSmallInputs  <- function(){
  predictionListStats <- c(2,1,0)
  experimentalResultStats <- c(2,1,0)
  epsilon = 1e-5
  pValue <-CausalR:::CalculateSignificanceUsingCubicAlgorithm(-1,predictionListStats, experimentalResultStats, epsilon) 
  checkEquals(pValue,1)
  pValue2 <-CausalR:::CalculateSignificanceUsingCubicAlgorithm(3,predictionListStats, experimentalResultStats, epsilon)
  checkEquals(pValue2,(1/3))
}

test_CalculateSignificanceUsingCubicAlgorithmProducesApproximatelyCorrectAnswerWithLargeInputs  <- function(){
  # Check that the answer from the cubic algorithm is within a certain tolerance of the answer obtained
  # using the quartic algorithm whose answer is exact.
  epsilon = 1e-5
  pValue3 <- CausalR:::CalculateSignificanceUsingCubicAlgorithm(5, c(10,10,20), c(10,10,20), epsilon)
  checkEqualsNumeric(pValue3, 0.080, tolerance = 1*10-3)
  pValue4 <- CausalR:::CalculateSignificanceUsingCubicAlgorithm(5, c(7,7,15), c(9,3,17), epsilon)
  checkEqualsNumeric(pValue4, 0.024, tolerance = 1*10-3)
}

test_CalculateWeightGivenValuesInThreeByThreeContingencyTableProducesTheCorrectOutput <- function(){
  threeByThreeContingencyTable <- c(1,1,0,1,1,1,0,1,1)
  logOfFactorialOfPredictionListStats <- c(log(factorial(2)),log(factorial(3)), log(factorial(2)))
  checkEquals(CausalR:::CalculateWeightGivenValuesInThreeByThreeContingencyTable(threeByThreeContingencyTable, logOfFactorialOfPredictionListStats), 24)
  threeByThreeContingencyTable2 <- c(1,1,2,1,1,1,0,1,1)
  logOfFactorialOfPredictionListStats2 <- c(log(factorial(4)),log(factorial(3)), log(factorial(2)))
  checkEquals(CausalR:::CalculateWeightGivenValuesInThreeByThreeContingencyTable(threeByThreeContingencyTable2, logOfFactorialOfPredictionListStats2), 144)
}
