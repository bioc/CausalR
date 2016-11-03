#' @title make predictions from CG
#' @description
#' Create a matrix of predictions for a particular hypothesis - the output
#' is a matrix containing the relationship between each node and the hypothesis.
#' The hypothesis provided will be the vertex id of one of the nodes in the network (as an integer). 
#' The signOfHypothesis variable should be a 1 or -1, indicating up/down regulation
#' @param hypothesisnode a hypothesis node
#' @param signOfHypothesis the direction of change of hypothesis node
#' @param network a computational causal graph
#' @param delta the number of edges across which the hypothesis should be followed
#' @param nodesInExperimentalData the number of nodes in experimental data
#' @return an matrix containing the relationship between each node and the hypothesis
#' @export
#' @concept CausalR
#' @examples
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' cg <- CreateCG(network)
#' MakePredictionsFromCG('NodeA', +1, cg, 2)

MakePredictionsFromCG <- function(hypothesisnode, signOfHypothesis, network, delta, nodesInExperimentalData = NULL) {
    
    signOfHypothesis <- as.integer(signOfHypothesis)
    delta <- as.integer(delta)
    
    if (is.numeric(hypothesisnode)) {
        hypothesis <- hypothesisnode
    } else {
        hypothesis <- which(igraph::V(network)$name == hypothesisnode)
    }
    
    # Check that the input hypothesis is a possible id. Ids are numbered from one to the number of vertices, so it is enough to check the hypothesis is
    # not less than one or greater than the number of vertices.
    if (is.na(hypothesis) || length(hypothesis) != 1 || hypothesis < 1 || hypothesis > igraph::gorder(network)) {
        stop("Couldn't create predictions - hypothesis ", hypothesisnode, " is not present in the network")
    }
    
    # Create subgraph containing the nodes connected to the hypothesis node
    subgraphOfConnectedNodes <- igraph::make_ego_graph(network, delta, hypothesis, "out")[[1]]
    
    hypothesisInSubgraph <- which(igraph::V(subgraphOfConnectedNodes)$name == igraph::V(network)[hypothesis]$name)
    
    # Get ids of connected nodes (these are will be the ids from the full network)
    idsOfConnectedNodes <- igraph::vertex_attr(subgraphOfConnectedNodes, "ID")
    
    if (!is.null(nodesInExperimentalData)) {
        # Get ids of nodes which are both in the subgraph and in the experimental data
        idsOfNodesInSubgraphAndData_main <- RemoveIDsNotInExperimentalData(idsOfConnectedNodes, nodesInExperimentalData)
        nNodes <- length(idsOfNodesInSubgraphAndData_main)
        
        # If there are no such nodes, the CCG will be empty, so return
        if (nNodes == 0) {
            return(NULL)
        }
        
        # Get the IDs of these nodes in the subgraph's numbering
        idsOfNodesInSubgraphAndData_sub <- FindIdsOfConnectedNodesInSubgraph(idsOfNodesInSubgraphAndData_main, subgraphOfConnectedNodes)
        
        # Create the matrix of causal relationships
        matrixOfCausalRelationships = matrix(0, nNodes, 2)
        matrixOfCausalRelationships[, 1] <- idsOfNodesInSubgraphAndData_main
        matrixOfCausalRelationships[, 2] <- GetMatrixOfCausalRelationships(hypothesisInSubgraph, subgraphOfConnectedNodes, idsOfNodesInSubgraphAndData_sub)
    } else {
        # Create the matrix of causal relationships
        nNodes <- length(idsOfConnectedNodes)
        matrixOfCausalRelationships = matrix(0, nNodes, 2)
        matrixOfCausalRelationships[, 1] <- idsOfConnectedNodes
        matrixOfCausalRelationships[, 2] <- GetMatrixOfCausalRelationships(hypothesisInSubgraph, subgraphOfConnectedNodes, 1:nNodes)
    }
    
    if (signOfHypothesis < 0) {
        matrixOfCausalRelationships[, 2] <- -matrixOfCausalRelationships[, 2]
    }
    
    return(matrixOfCausalRelationships)
} 
