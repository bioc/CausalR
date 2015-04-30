#' @title  run ScanR
#' @description 
#' This function will return nodes regulated by the given hypothesisGene
#' @param PPInet PPInetwork
#' @param Expressiondata Expressiondata
#' @param NumberOfDeltaToScan Iteratively scan for 1 to NumberOfDeltaToScan delta values
#' @param topNumGenes A value to select top genes to report (typically top 100 genes)
#' @param correctPredictionsThreshold Minimal score for p-values calculation. Hypotheses with scores below this value will get NAs for p-value and enrichment p-value. The usual default is -inf within the RankTheHypotheses function, where it is employed.
#' @return returns list of genes from each delta scan run
#' @export
#' @examples
#' NumberOfDeltaToScan=2
#' topNumGenes=4
#' correctPredictionsThreshold=1
#' expData<- system.file(package="CausalR", "extdata", "testData.txt")
#' network <- system.file(package="CausalR", "extdata", "testNetwork.sif")
#' runSCANR(network, expData,NumberOfDeltaToScan,topNumGenes, correctPredictionsThreshold)

runSCANR <-function(PPInet, Expressiondata,NumberOfDeltaToScan=5,topNumGenes=150, correctPredictionsThreshold=1){
testList <- list()
for (j in 1:NumberOfDeltaToScan){
	rankedHypo<- runRankHypothesis(PPInet,Expressiondata, j,    correctPredictionsThreshold) 
	testList[[j]] <-row.names(rankedHypo)[1:topNumGenes]
	}
commonGenes<-Reduce(intersect, testList)
testList[[j+1]]<-commonGenes

return(testList)
}