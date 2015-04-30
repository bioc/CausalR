#' @title remove IDs not in experimental data
#' @description
#' Takes in a list of connected nodes and removes those not in the experimental data.
#' @param connectedNodes a list of connected nodes
#' @param nodesInExperimentalData a list of nodes in the experimental data
#' @return connectedNodesInExperimentalData a list of connected nodes with the redundant nodes removed


RemoveIDsNotInExperimentalData <- function(connectedNodes, nodesInExperimentalData){

connectedNodesInExperimentalData <- intersect(connectedNodes, nodesInExperimentalData)
return(connectedNodesInExperimentalData)
}