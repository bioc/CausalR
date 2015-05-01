#' @title make predictions
#' @description
#' Creates a matrix of predictions for a particular hypothesis.
#' The output is an array containing the relationship between each node and the hypothesis.
#' The hypothesis provided will be the vertex id of one of the nodes in the network
#' (as an integer node ID or name, including + or - for up/down regulation in the case of a CCG).
#' The signOfHypothesis variable should be a 1 or -1, indicating up/down regulation.

#' @export
#' @param hypothesisnode the node in the causal graph from which predictions should be made.  Can be either a (numerical) node ID or a (string) node name.
#' @param signOfHypothesis whether the hypothesis node is up- or down-regulated. Should be +1 or -1.
#' @param network a (Computational) Causal Graph, as an igraph.
#' @param delta the distance to search within the causal graph.
#' @param nodesInExperimentalData optional. Nodes to include in the output. Should be a list of node IDs.
#' @return a matrix of predictions for the given particular hypothesis
#' @examples
#' network <- system.file(package="CausalR", "extdata", "testNetwork.sif")
#' ccg <- CreateCCG(network)
#' predictions <- MakePredictions("NodeA", +1, ccg, 2)

MakePredictions <- function(hypothesisnode, signOfHypothesis, network, delta, nodesInExperimentalData = NULL){
  # Create a matrix of predictions for a particular hypothesis.
  # The output is an array containing the relationship between each node and the hypothesis.
  # The hypothesis provided will be the vertex id of one of the nodes in the network
  # (as an integer node ID or name, including + or - for up/down regulation in the case of a CCG).
  # The signOfHypothesis variable should be a 1 or -1, indicating up/down regulation.
  
  #if (network$isCCG){
  predictions <- MakePredictionsFromCCG(hypothesisnode,    signOfHypothesis, network, delta, nodesInExperimentalData)
  #}
  #else{
  #  predictions <- MakePredictionsFromCG(hypothesisnode, signOfHypothesis, network, delta, nodesInExperimentalData)
  #}
  return(predictions)
}