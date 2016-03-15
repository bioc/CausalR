#' @title  run ScanR
#' @description 
#' This function will return nodes regulated by the given hypothesisGene
#' @param network Computational Causal Graph, as an igraph.
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. The results is an n x 2 matrix; where the first column contains the node ids of the nodes in the network that the results refer to. The second column contains values indicating the direction of regulation in the results - (+)1 for up, -1 for down and 0 for insignificant amounts of regulation.
#' @param NumberOfDeltaToScan Iteratively scan for 1 to NumberOfDeltaToScan delta values
#' @param topNumGenes A value to select top genes to report (typically top 100 genes)
#' @param correctPredictionsThreshold Minimal score for p-values calculation. Hypotheses with scores below this value will get NAs for p-value and enrichment p-value. The usual default is -inf within the RankTheHypotheses function, where it is employed.
#' @param doParallel A flag for running RankTheHypothesis in parallel mode.
#' @param numCores Number of cores to use if using parallel mode. If the default value of NULL is used, it will attempt to detect the number of cores available and use all of them bar one.
#' @return returns list of genes from each delta scan run
#' @export
#' @concept CausalR
#' @examples
#' NumberOfDeltaToScan <- 2
#' topNumGenes <- 4
#' #get path to example network file
#' network <- system.file(package = 'CausalR', 'extdata', 'testNetwork.sif')
#' #create ccg
#' ccg <-  CreateCCG(network)
#' #get path to example experimental data
#' fileName<- system.file(package = 'CausalR', 'extdata', 'testData.txt')
#' #read in experimetal data
#' expData <-  ReadExperimentalData(fileName, ccg)
#' #run in single threaded mode
#' runSCANR(ccg, expData, NumberOfDeltaToScan, topNumGenes)
#' #run in parallel mode
#' runSCANR(ccg, expData, NumberOfDeltaToScan, topNumGenes,
#'          doParallel = TRUE, numCores = 2)

runSCANR <- function(network, experimentalData, NumberOfDeltaToScan = 5, topNumGenes = 150, correctPredictionsThreshold = 1, doParallel=FALSE, numCores=NULL) {
    
    testList <- list()
    
    for (j in 1:NumberOfDeltaToScan) {
        rankedHypo <- RankTheHypotheses(network, experimentalData, j, correctPredictionsThreshold, doParallel=doParallel, numCores=numCores)
        testList[[j]] <- row.names(rankedHypo)[1:topNumGenes]
    }
    
    commonGenes <- Reduce(intersect, testList)
    testList[[j + 1]] <- commonGenes
    
    return(testList)
} 
