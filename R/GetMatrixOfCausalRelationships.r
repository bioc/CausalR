#' @title compute causal relationships matrix
#' @description 
#' Get a matrix of causal relationships from the network and the IDs of connected nodes
#' @param hypothesis a hypothesis node
#' @param network a CCG network
#' @param idsOfConnectedNodesFromSubgraph a list of connected nodes in the subgraph of interest
#' @return  causal relationships matrix

GetMatrixOfCausalRelationships <- function(hypothesis, network, idsOfConnectedNodesFromSubgraph){

  nNodes <- length(idsOfConnectedNodesFromSubgraph)
  
  causalRelationships <- array(0, nNodes)
  
  for (iNode in 1:nNodes){
    if (hypothesis == idsOfConnectedNodesFromSubgraph[iNode]){
      #  Hypothesis node's interaction with itself has to be 1.
      causalRelationships[iNode] <- 1
    }
    else{
      shortestPaths <- get.all.shortest.paths(network, hypothesis, idsOfConnectedNodesFromSubgraph[iNode], "out")
      numberOfShortestPaths <- length(shortestPaths[[1]])
      # Determine interaction type for first path
      interactionValue <- DetermineInteractionTypeOfPath(network, shortestPaths[[1]][1])
      
      if (numberOfShortestPaths > 1){
        # if there is more than one shortest path, two things can happen:
        # 	1. they could all agree, in which case we can take any of the values in the vector to populate the data frame
        # 	2. they could disagree, in this case the interaction given the value 0 (as opposed to 1/-1 for activation/inhibition.
        
        for (counter in 2:numberOfShortestPaths){
          interactionValueForAlternativePath <- DetermineInteractionTypeOfPath(network, shortestPaths[[1]][counter])
          if (interactionValueForAlternativePath != interactionValue){
            interactionValue <- 0
            break()
          }
        }  
      }
	  causalRelationships[iNode] <- interactionValue
    }
  }
  
  return(causalRelationships) 
}