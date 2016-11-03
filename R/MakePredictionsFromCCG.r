#' @title make predictions from CCG
#' @description
#' Create a matrix of predictions for a particular hypothesis starting from a network 
#' with separate nodes for up- and down-regulation (+ve and -ve).
#' The output is an array containing the relationship between each node and the hypothesis.
#' The hypothesis provided will be the vertex id of one of the nodes in the network
#' (as an integer or name including + or - for up/down regulation).
#' The signOfHypothesis variable should be a 1 or -1, indicating up/down regulation.
#' (It generally shouldn't be necessary to reverse the sign of a node when working from
#' a CCG, but this facility is included for consistency with MakePredictionsFromCG)

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
#' ccg <- CreateCCG(network)
#' MakePredictionsFromCCG('NodeA', +1, ccg, 2)


MakePredictionsFromCCG <- function(hypothesisnode, signOfHypothesis, network, delta, nodesInExperimentalData = NULL) {
    # Create a matrix of predictions for a particular hypothesis starting from a network with separate nodes for up- and down-regulation (+ve and -ve).
    # The output is an array containing the relationship between each node and the hypothesis.  The hypothesis provided will be the vertex id of one of
    # the nodes in the network (as an integer or name including + or - for up/down regulation).  The signOfHypothesis variable should be a 1 or -1,
    # indicating up/down regulation.  (It generally shouldn't be necessary to reverse the sign of a node when working from a CCG, but this facility is
    # included for consistency with MakePredictionsFromCG)
    
    signOfHypothesis <- as.integer(signOfHypothesis)
    delta <- as.integer(delta)
    
    if (is.numeric(hypothesisnode)) {
        hypothesis <- hypothesisnode
    } else {
        hypothesis <- GetNodeID(network, hypothesisnode)
    }
    
    # Check that the input hypothesis is a possible id. Ids are numbered from one to the number of vertices, so it is enough to check the hypothesis is
    # not less than one or greater than the number of vertices.
    if (is.na(hypothesis) || length(hypothesis) != 1 || hypothesis < 1 || hypothesis > igraph::gorder(network)) {
        stop("Couldn't create predictions - hypothesis ", hypothesisnode, " is not present in the network")
    }
    nNodes <- igraph::gorder(network)
    
    # Array for the distance from the hypothesis to each node. Default value of (delta+1), i.e. too far away to matter
    distance <- array(delta + 1, nNodes)
    
    # Number of nodes at each distance from the hypothesis
    n <- array(0, delta + 1)
    
    # Interaction of the hypothesis node with itself
    distance[hypothesis] <- 0
    n[1] <- 1
    
    if (delta > 0) {
        for (currentStep in 1:delta) {
            # As of igraph V1.0.0 functions return sequences instead of numeric ids. as.vector() converts these back to ids.
            nn <- as.vector(igraph::ego(network, currentStep, hypothesis, "out")[[1]])  # All nodes out to currentStep
            n[currentStep + 1] <- length(nn)
            if (n[currentStep + 1] > n[currentStep]) {
                # For all of the nodes which are new on this step, set distance to currentStep
                distance[nn[n[currentStep] + 1:n[currentStep + 1]]] <- currentStep
            }
        }
    }
    
    # Now process the distances to work out the interactions.
    interactions <- array(NA, nNodes/2)
    interactions[distance[1:(nNodes/2)] < distance[(nNodes/2 + 1):nNodes]] <- 1  # NodeX+ is closer than NodeX-
    interactions[distance[1:(nNodes/2)] > distance[(nNodes/2 + 1):nNodes]] <- -1  # NodeX+ is further than NodeX-
    interactions[distance[1:(nNodes/2)] <= delta & distance[1:(nNodes/2)] == distance[(nNodes/2 + 1):nNodes]] <- 0  # NodeX+ and NodeX- are same distance
    # interactions[distance[1:(nNodes/2)] > delta & distance[(nNodes/2+1):nNodes] > delta] = NA # Neither NodeX+ and NodeX- are reached within delta
    # (don't actually need to do this as NA is the default value)
    
    if (is.null(nodesInExperimentalData)) {
        matrixOfCausalRelationships <- matrix(c((1:(nNodes/2))[!is.na(interactions)], interactions[!is.na(interactions)]), ncol = 2)
    } else {
        matrixOfCausalRelationships <- matrix(c(nodesInExperimentalData, interactions[nodesInExperimentalData]), ncol = 2)
        matrixOfCausalRelationships <- matrix(matrixOfCausalRelationships[!is.na(matrixOfCausalRelationships[, 2]), ], ncol = 2)
    }
    
    if (signOfHypothesis < 0) {
        matrixOfCausalRelationships[, 2] <- -matrixOfCausalRelationships[, 2]
    }
    
    if (!nrow(matrixOfCausalRelationships) == 0) {
        return(matrixOfCausalRelationships)
    } else {
        return(NULL)
    }
} 
