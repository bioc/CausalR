#' @title find Ids of connected nodes in subgraph
#' @description
#' Adds the IDs of the connected nodes in a subgraph to an existing list. 
#' Given the IDs of connected nodes in the full network, this function will find
#' the corresponding IDs in the subgraph
#' 

#' @param idsOfConnectedNodes a list of connected nodes in the full graph
#' @param subgraphOfConnectedNodes a subgraph 
#' @return a list of connected nodes in the subgraph

FindIdsOfConnectedNodesInSubgraph <- function(idsOfConnectedNodes, subgraphOfConnectedNodes){

  
  numConnectedNodes <- length(idsOfConnectedNodes)
  idsOfConnectedNodesInSubgraph <- array(0,length(idsOfConnectedNodes))
  ids <- V(subgraphOfConnectedNodes)$ID
  
  for (i in 1:numConnectedNodes){
    idsOfConnectedNodesInSubgraph[i] <- which(ids == idsOfConnectedNodes[i])
  }
  
  return(idsOfConnectedNodesInSubgraph)
}