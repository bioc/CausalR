#' @title Write all explained nodes to Sif file
#' @description
#' Outputs networks of all explained nodes in .sif file format, named by node name with sign of regulation, each with a corresponding annotation file for producing visualisations using Cytoscape.  
#' @param scanResults a results object produced by ScanR
#' @param network a computational causal graph
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. 
#' @param delta the number of edges across which the hypothesis should be followed, the setting should be that used to generate the input ScanR object.
#' @param correctlyExplainedOnly if TRUE network files will only be produced for correctly explained nodes. If FALSE network files will be produced for each of correctly explained, incorrectly explained and ambiguously explained nodes.
#' @param quiet a flag to suppress output to console. FALSE by default.
#' @return files containing paths from hypothesis node to explained nodes in sif format and corresponding annotation (_anno.txt) files
#' @export
#' @concept CausalR
#' @examples
#' networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
#' network <- CreateCCG(networkFile)
#' experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
#' experimentalData <- ReadExperimentalData(experimentalDataFile, network)
#' delta <- 2
#' scanResults <- runSCANR(network, experimentalData, numberOfDeltaToScan = delta,
#'     topNumGenes = 2, writeResultFiles = FALSE, writeNetworkFiles = "none",
#'     quiet = FALSE, doParallel = TRUE, numCores = 2)
#' WriteAllExplainedNodesToSifFile(scanResults, network, experimentalData, delta,
#'     correctlyExplainedOnly = TRUE, quiet = TRUE)

WriteAllExplainedNodesToSifFile <- function(scanResults, network, experimentalData, delta, correctlyExplainedOnly = TRUE, quiet = TRUE){
    
    NumCommonNodes <-length(scanResults$commonNodes)
    if (NumCommonNodes == 0) {
        stop("ScanR returned no common nodes at given delta")
    }
    
    for(i in 1:NumCommonNodes){
        
        # get current hypothesis node 'i' name concatenated with it's sign of regulation
        
        NodeiWithSign <- scanResults$commonNodes[i]    
        
        # separate NodeiWithSign into nodename and sign
        
        Nodeiname <- substr(NodeiWithSign, 1, nchar(NodeiWithSign)-1)
        NodeiSign <- substr(NodeiWithSign, nchar(NodeiWithSign), nchar(NodeiWithSign))
        
        if (NodeiSign=='+') {
            signOfHypothesis <- paste(+ 1, sep='')
        } else {
            signOfHypothesis <- paste(- 1, sep='')
        } 
        
        WriteExplainedNodesToSifFile(hypothesisnode = Nodeiname, signOfHypothesis , network, experimentalData, delta, outputFilesName = NodeiWithSign, correctlyExplainedOnly=TRUE, quiet=TRUE)
    } # end for
}
