#' @title create network from table
#' @description
#' Creates a network from an internal data table created from a .sif file:
#' this function converts the data read in from the .sif file into 
#' an igraph in R.
#' 

#' @param dataTable the data table containing the information read in from the .sif file representing the network.
#' @return an igraph network


CreateNetworkFromTable <- function(dataTable){


matrixOfInteractions <- as.matrix(dataTable)
# Use the first and third colums (names of genes that define an edge) to build the network
network <- graph.edgelist(matrixOfInteractions[,-2], TRUE)
return(network)
}