#' @title add IDs to vertices

#' @description 
#' Adds the IDs as a vertex property to 
#' the vertices in the network. Used when creating
#' sub-networks where the new nodes will retain the IDs from their original
#' network

#' @param network the network to which the IDs are to be added
#' @return  network with IDs added

AddIDsToVertices <- function(network){


numNodes <- vcount(network)
network <- set.vertex.attribute(network,"ID", 1:numNodes, 1:numNodes)
}