#' @title plot graph with node names
#' @description
#' Plots an igraph with the node names. Plots a igraph to the screen displaying the names of the nodes input rather than R's internal numbering.

#' @param igraph internal an igraph representation of an interaction network
#' @return network visualisation
#' @export
#' @concept CausalR
#' @examples
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' ccg <- CreateCCG(network)
#' PlotGraphWithNodeNames(ccg)




PlotGraphWithNodeNames <- function(igraph) {
    
    graphics::plot(igraph, vertex.label = igraph::V(igraph)$name)
    
} 
