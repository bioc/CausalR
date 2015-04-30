#' @title get node name
#' @description
#' Returns the node name from one or more node IDs, or substitute node names for node IDs,
#' given in first column of a matrix typically of  predictions or experimental data
#' @param network Built from igraph
#' @param nodeID a node ID or a matrix containing node IDs in its first column
#' @param signed whether or not the node name should be signed. Setting this value to TRUE gives a signed name indicating whether the gene is up or down regulated in the network
#' @return a node name or a vector of node names depending if the input is an matrix.
#' @export
#' @examples
#' network <- system.file(package="CausalR", "extdata", "testNetwork.sif")
#' ccg = CreateCCG(network)
#' nodeID <- 10
#' GetNodeName(ccg, nodeID)


GetNodeName <- function(network, nodeID, signed=FALSE){

  if (is.matrix(nodeID) || is.data.frame(nodeID)){
    if (network$isCCG){
      name <- nodeID
      if (signed){
        name[,1] <- V(network)$name[nodeID[,1]]
      }
      else{
        name[,1] <- V(network)$unsignedName[nodeID[,1]]
      }
      
    }
    else{
      name <- nodeID
      name[,1] <- V(network)$name[nodeID[,1]]
    }
  }
  else{
    if (network$isCCG){
      if (signed){
        name <- V(network)$name[nodeID]
      }
      else{
        name <- V(network)$unsignedName[nodeID]
      }
    }
    else{
      name <- V(network)$name[nodeID]
    }
  }

  return(name)
}
