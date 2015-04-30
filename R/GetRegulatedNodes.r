#' @title get regulated nodes
#' @description 
#' This function will compute the nodes regulated by the given hypothesis gene and write the results to a file
#' @param PPInet a protein-protein interaction network
#' @param Expressiondata a table of observed gene expression data
#' @param delta the number of edges to follow along the network. This should typically be between 1 and 5 dependent on network size/topology
#' @param hypothesisGene the name of the hypothesis gene
#' @param signOfHypothesis the sign of action expected from the hypothesis, +1 for up regulation, -1 for down 
#' @param outputfile the file to which the results should be written
#' @return Nodes regulated by hypothesis gene

 
GetRegulatedNodes <-function(PPInet, Expressiondata,delta, hypothesisGene="",signOfHypothesis=1,outputfile=""){
ccg = CreateCCG(PPInet)
experimentalData = ReadExperimentalData(Expressiondata, ccg)
#rankedHypothesis=RankTheHypotheses(ccg, experimentalData, delta)
ndelta<-delta+1
predictions <- MakePredictionsFromCCG(hypothesisGene,signOfHypothesis,ccg,ndelta)
x<-GetNodeName(ccg,CompareHypothesis(predictions,experimentalData))
write.table(x, file=outputfile, sep="\t", col.names=TRUE, quote=FALSE)

}
