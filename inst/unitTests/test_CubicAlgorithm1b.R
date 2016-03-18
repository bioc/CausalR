# A test file for the (faster) 1b cubic algorithm functions

test_GetApproximateMaximumDValueFromThreeByTwoContingencyTable <- function() {
    threeByTwoContingencyTable <- c(1, 3, 3, 2, 1, 4)
    predictionListStats <- c(6, 6, 6)
    logOfFactorialOfPredictionListStats <- lfactorial(c(6, 6, 6))
    checkEquals(CausalR:::GetApproximateMaximumDValueFromThreeByTwoContingencyTable(threeByTwoContingencyTable, predictionListStats, logOfFactorialOfPredictionListStats), 
        108000)
}

test_GetMaxDValueForAThreeByTwoFamily <- function() {
    predictionListStats <- c(2, 4, 1)
    logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
    returnlog <- FALSE
    checkEquals(CausalR:::GetMaxDValueForAThreeByTwoFamily(1, 3, 1, 3, 2, predictionListStats, logOfFactorialOfPredictionListStats, returnlog = FALSE), 
        24)
}

test_TheCorrectTotalWeightForAllContingencyTablesIsCalculated <- function() {
    experimentalDataStats <- c(2, 3, 4)
    checkEquals(CausalR:::CalculateTotalWeightForAllContingencyTables(experimentalDataStats), 1260)
    checkEquals(CausalR:::CalculateTotalWeightForAllContingencyTables(experimentalDataStats, returnlog = TRUE), log(1260))
}

test_CorrectWeightsAboveHypothesisScoreAreReturnedWithDifferentScores <- function() {
    rowAndColumnSumValuesForThreeByTwoSubmatrix <- c(1, 3, 1, 3, 2)
    predictionListStats <- c(2, 4, 1)
    experimentalDataStats <- c(3, 2, 2)
    logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
    hypothesisScore <- -2
    epsilon <- 1e-05
    logepsDMax <- log(24 * epsilon)
    logDMax <- log(24)
    weightStore <- c(0, 0)
    checkEquals(CausalR:::GetWeightsAboveHypothesisScoreForAThreeByTwoTable(weightStore, 1, 3, 1, 3, 2, predictionListStats, experimentalDataStats, 
        logOfFactorialOfPredictionListStats, hypothesisScore, logepsDMax, logDMax), c(3, 10/3))
    hypothesisScore2 <- 0
    checkEquals(CausalR:::GetWeightsAboveHypothesisScoreForAThreeByTwoTable(weightStore, 1, 3, 1, 3, 2, predictionListStats, experimentalDataStats, 
        logOfFactorialOfPredictionListStats, hypothesisScore2, logepsDMax, logDMax), c(2, 10/3))
    hypothesisScore3 <- 2
    checkEquals(CausalR:::GetWeightsAboveHypothesisScoreForAThreeByTwoTable(weightStore, 1, 3, 1, 3, 2, predictionListStats, experimentalDataStats, 
        logOfFactorialOfPredictionListStats, hypothesisScore3, logepsDMax, logDMax), c(1, 10/3))
    
}

test_CalculateSignificanceUsingCubicAlgorithm1bProducesExactAnswerWithSmallInputs <- function() {
    predictionListStats <- c(2, 1, 0)
    experimentalResultStats <- c(2, 1, 0)
    epsilon = 1e-05
    pValue <- CausalR:::CalculateSignificanceUsingCubicAlgorithm1b(-1, predictionListStats, experimentalResultStats, epsilon)
    checkEquals(pValue, 1)
    pValue2 <- CausalR:::CalculateSignificanceUsingCubicAlgorithm1b(3, predictionListStats, experimentalResultStats, epsilon)
    checkEquals(pValue2, (1/3))
}


test_CubicAlgorithm1bProducesApproximatelyCorrectAnswerWithLargeInputs <- function() {
    # Check that the answer from the faster cubic algorithm is within a certain tolerance of the answer obtained using the quartic algorithm whose
    # answer is exact.
    epsilon = 1e-05
    pValue3 <- CausalR:::CalculateSignificanceUsingCubicAlgorithm1b(5, c(10, 10, 20), c(10, 10, 20), epsilon)
    checkEqualsNumeric(pValue3, 0.08, tolerance = 1 * 10 - 3)
    pValue4 <- CausalR:::CalculateSignificanceUsingCubicAlgorithm1b(5, c(7, 7, 15), c(9, 3, 17), epsilon)
    checkEqualsNumeric(pValue4, 0.024, tolerance = 1 * 10 - 3)
} 
