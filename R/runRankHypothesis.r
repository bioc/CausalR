#' @title run rank the hypothesis
#' @description 
#' A top level function that used to run CausalR
#' @param PPInet PPInet is the PPI interaction file
#' @param Expressiondata observed gene expression data
#' @param delta the number of links to follow from any hypothesis no. Dedepending on network size/topology, this value typically ranges between 1 and 5
#' @param correctPredictionsThreshold Minimal score for p-values calculation. Hypotheses with scores below this value will get NAs for p-value and enrichment p-value. The usual default is -inf within the RankTheHypotheses function, where it is employed.
#' @return rankedHypothesis table of results produced by the algorithm

runRankHypothesis <- function(PPInet, Expressiondata,delta, correctPredictionsThreshold){  

ccg = CreateCCG(PPInet)
experimentalData = ReadExperimentalData(Expressiondata, ccg)
rankedHypothesis=RankTheHypotheses(ccg, experimentalData, delta, correctPredictionsThreshold)

return(rankedHypothesis)

}
