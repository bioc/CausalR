# A test suite for ScoreHypothesis and CompareHypothesis functions

test_ScoreHypothesisProducesTheCorrectScoreWhenAllNodesInPredictionsAreInTheExperimentalData <- function() {
    matrixOfPredictions <- matrix(c(1, 3, 1, -1), nrow = 2)
    matrixOfExperimentalData <- matrix(c(1, 2, 3, 1, -1, 0), nrow = 3)
    # 1 is correct (predicted +, observed +) 3 doesn't appear in the table (predicted -, observed 0)
    scoreBreakdown <- CausalR:::ScoreHypothesis(matrixOfPredictions, matrixOfExperimentalData)
    checkEquals(scoreBreakdown[1], 1)
    checkEquals(scoreBreakdown[2], 1)
    checkEquals(scoreBreakdown[3], 0)
    checkEquals(scoreBreakdown[4], 0)
    
    matrixOfExperimentalData2 <- matrix(c(1, 2, 3, -1, -1, 1), nrow = 3)
    scoreBreakdown <- CausalR:::ScoreHypothesis(matrixOfPredictions, matrixOfExperimentalData2)
    checkEquals(scoreBreakdown[1], -2)
    checkEquals(scoreBreakdown[2], 0)
    checkEquals(scoreBreakdown[3], 2)
    checkEquals(scoreBreakdown[4], 0)
}

test_ScoreHypothesisProducesTheCorrectScoreWhenThePredictionsContainsANodeNotInTheExperimentalData <- function() {
    matrixOfPredictions <- matrix(c(1, 3, 1, -1), nrow = 2)
    matrixOfExperimentalData <- matrix(c(1, 2, 1, -1), nrow = 2)
    scoreBreakdown <- CausalR:::ScoreHypothesis(matrixOfPredictions, matrixOfExperimentalData)
    checkEquals(scoreBreakdown[1], 1)
    checkEquals(scoreBreakdown[2], 1)
    checkEquals(scoreBreakdown[3], 0)
    checkEquals(scoreBreakdown[4], 0)
}

test_ScoreHypothesisProducesTheCorrectScoreWhenNodesAreMisMatched <- function() {
    matrixOfPredictions <- matrix(c(1, 2, 4, 5, -1, 1, 1, -1), nrow = 4)
    matrixOfExperimentalData <- matrix(c(1, 3, 5, 1, -1, 1), nrow = 3)
    scoreBreakdown <- CausalR:::ScoreHypothesis(matrixOfPredictions, matrixOfExperimentalData)
    checkEquals(scoreBreakdown[1], -2)
    checkEquals(scoreBreakdown[2], 0)
    checkEquals(scoreBreakdown[3], 2)
    checkEquals(scoreBreakdown[4], 0)
}

test_ScoreHypothesisCorrectlyIdentifiesAmbiguousNodes <- function() {
    matrixOfPredictions <- matrix(c(1, 2, 3, 4, 5, -1, 0, 1, 1, 0), nrow = 5)
    matrixOfExperimentalData <- matrix(c(1, 3, 5, 1, 0, 1), nrow = 3)
    # 1 is incorrect 3 doesn't appear in the table (predicted +, observed 0) 5 is ambiguous (predicted conflicted, observed +)
    scoreBreakdown <- CausalR:::ScoreHypothesis(matrixOfPredictions, matrixOfExperimentalData)
    checkEquals(scoreBreakdown[1], -1)
    checkEquals(scoreBreakdown[2], 0)
    checkEquals(scoreBreakdown[3], 1)
    checkEquals(scoreBreakdown[4], 1)
}

test_ScoreHypothesisWorksWhenNoOverlap <- function() {
    matrixOfPredictions <- matrix(c(1, 2, 3, 4, 5, -1, 0, 1, 1, 0), nrow = 5)
    matrixOfExperimentalData <- matrix(c(6, 7, 8, 1, 0, 1), nrow = 3)
    scoreBreakdown <- CausalR:::ScoreHypothesis(matrixOfPredictions, matrixOfExperimentalData)
    checkTrue(all(scoreBreakdown == 0))
}

test_CompareHypothesisWorksWhenNoOverlap <- function() {
    matrixOfPredictions <- matrix(c(1, 2, 3, 4, 5, -1, 0, 1, 1, 0), nrow = 5)
    matrixOfExperimentalData <- matrix(c(6, 7, 8, 1, 0, 1), nrow = 3)
    comparison <- CausalR:::CompareHypothesis(matrixOfPredictions, matrixOfExperimentalData)
    checkEquals(dim(comparison)[1], 0)
} 
