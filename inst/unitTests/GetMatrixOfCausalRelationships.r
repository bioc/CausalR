GetMatrixOfCausalRelationships <- function(hypothesis, network, idsOfConnectedNodesFromSubgraph){
  # Get matrix of Causal Relationships from the network and the IDs of connected nodes
  
  # *** SVN version information ***
  # $Date: 2013-01-03 18:03:15 +0000 (Thu, 03 Jan 2013) $
  # $Revision: 366 $
  # $Author: rild $
  # $HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkPredictions/GetMatrixOfCausalRelationships.r $
  
  # Copyright 2012 GSK. All rights reserved
  
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