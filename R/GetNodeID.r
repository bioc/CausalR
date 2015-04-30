#' @title get CCG node ID
#' @description
#' Returns the CCG node ID from a node name or a vector of node names and a given direction of regulation.
#' @param network a CCG object
#' @param nodename the node name, or names, for which the ID is required
#' @param direction the direction of regulation of the required node or nodes. Maybe +1 (default) or -1.
#' @return a scalar or vector containing the node ID or IDs requested

GetNodeID <- function(network, nodename, direction=1){
  
  n <- length(nodename)
  if (n>1){
    nodeID <- array(0,n)
    for (i in 1:n){
      nodeID[i] <- GetNodeID(network, nodename[i], direction)
    }
  }
  else{
    # If we're given the full name, return the associated node ID
    nodeID <- which(V(network)$name == nodename)
    
    if (length(nodeID)==0 && network$isCCG){
      # nodename doesn't match any of the signed node names, try the unsigned names
      if (direction > 0){
        nodeID <- which(V(network)$unsignedName == nodename)[1]
      }
      else{
        nodeID <- which(V(network)$unsignedName == nodename)[2]
      }
    }
  }
  
  return(nodeID)
  
}