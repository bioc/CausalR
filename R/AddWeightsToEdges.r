#' @title 
#' add weights to edges
#' @description
#' Adds weight information to the edges of given network (1 for activation and -1 for inhibition)

#' @param network an igraph constructed from the original .sif file
#' @param tableOfInteractions a column of the corresponding .sif file indicating the direction of activation/interaction
#' @return an augmented network

AddWeightsToEdges<- function(network, tableOfInteractions){
	
matrixOfInteractions <- as.matrix(tableOfInteractions)
# matrixOfInteractions will be used to define an edge property called "Weight" for 
# each edge. If the value in matrixOfInteractions is "Activates/Activation" the value of the weight is +1. 
# Otherwise if the value is "Inhibits/Inhibition", the value of weight will be -1.
matrixOfWeights <- GetWeightsFromInteractionInformation(matrixOfInteractions)

edgesOfNetwork <- E(network)
network = set.edge.attribute(network, "Weight", edgesOfNetwork, matrixOfWeights)	 

return(network)
}
