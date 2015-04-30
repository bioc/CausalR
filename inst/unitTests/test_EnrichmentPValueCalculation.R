# A test suite for EnrichmentPValueCalculation
library(igraph)


test_CalculateEnrichmentPValueProducesTheCorrectValueWhenResultsAndPredictionsContainTheSameIDs <- function(){
  predictions <- matrix(c(1,2,3,4,5, 1, 0, 0, 1,  0), ncol = 2)
  results<- matrix(c(1,2,3,4,5, 1, 0, 0, -1,  1), ncol = 2)
  checkEquals(CausalR:::CalculateEnrichmentPValue(predictions, results), 0.3)
}

test_CalculateEnrichmentPValueProducesTheCorrectValueWhenPredictionsAndResultsIDsAreMisMatched <- function(){
  predictions <- matrix(c(1,2,4,5,1,-1,-1,1), ncol = 2)
  results <- matrix(c(1,2,3,4,1,-1,0,0), ncol = 2)
  checkEquals(CausalR:::CalculateEnrichmentPValue(predictions, results), 0.5)
}

test_GetSetOfDifferentiallyExpressedGenesIdentifiesTheCorrectGenes <- function(){
  results <- matrix(c(1,2,3,4, 1,  1,  0,  -1), ncol = 2)
  differentiallyExpressedGenes <- CausalR:::GetSetOfDifferentiallyExpressedGenes(results)
  checkEquals(length(differentiallyExpressedGenes), 3)
  checkEquals(differentiallyExpressedGenes[1], 1)
  checkEquals(differentiallyExpressedGenes[2], 2)
  checkEquals(differentiallyExpressedGenes[3], 4)
}

test_GetSetOfSignificantPredictionsIdentifiesTheCorrectValues <- function(){
  predictions <- matrix(c(1,2,3,4, 1, 0,  -1, -1), ncol = 2)
  significantPredictions <- CausalR:::GetSetOfSignificantPredictions(predictions)
  checkEquals(length(significantPredictions), 3)
  checkEquals(significantPredictions[1], 1)
  checkEquals(significantPredictions[2], 3)
  checkEquals(significantPredictions[3], 4)
}