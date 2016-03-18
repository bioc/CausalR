# Test functions for the MakePredictionsFromCCG functionality

# Copyright: Copyright 2012 GSK. All rights reserved

# *** SVN version information *** $Date: 2015-12-15 11:36:58 +0000 (Tue, 15 Dec 2015) $ $Revision: 553 $ $Author: pipm $ $HeadURL:
# https://stevenagefs:7777/7412/trunk/NetworkPredictions/test_CausalReasoningPredictions.R $

CreateSimpleNetworkForTest_ComputationalCausalGraphCreator <- function() {
    network <- igraph::graph_from_literal("node0" - +"node1", "node0" - +"node2")
    network <- igraph::set_edge_attr(network, "Weight", 1, 1)
    network <- igraph::set_edge_attr(network, "Weight", 2, -1)
    network <- igraph::set_vertex_attr(network, "ID", 1:3, 1:3)
    
    return(network)
}

test_RemoveIDsNotInExperimentalDataProducesTheCorrectOutputList <- function() {
    connectedNodes <- c(1, 2, 3, 4, 5, 6)
    expData <- matrix(c(1, 2, 5, 6), ncol = 2)
    connectedNodesInExpData <- CausalR:::RemoveIDsNotInExperimentalData(connectedNodes, expData)
    checkEquals(length(connectedNodesInExpData), 4)
    checkEquals(connectedNodesInExpData[1], 1)
    checkEquals(connectedNodesInExpData[2], 2)
    checkEquals(connectedNodesInExpData[3], 5)
    checkEquals(connectedNodesInExpData[4], 6)
}

test_DetermineInteractionTypeOfPathProducesTheCorrectOutput <- function() {
    test_network <- CreateSimpleNetworkForTest_ComputationalCausalGraphCreator()
    path <- igraph::shortest_paths(test_network, 1, 2, "out")
    # Fix for igraph 1.0.1
    if (!is.null(path$vpath)) {
        path <- lapply(path$vpath, as.vector)
    }
    interaction1 <- CausalR:::DetermineInteractionTypeOfPath(test_network, path)
    checkEquals(interaction1, 1)
    path2 <- igraph::shortest_paths(test_network, 1, 3, "out")
    # Fix for igraph 1.0.1
    if (!is.null(path2$vpath)) {
        path2 <- lapply(path2$vpath, as.vector)
    }
    interaction2 <- CausalR:::DetermineInteractionTypeOfPath(test_network, path2)
    checkEquals(interaction2, -1)
}

test_DetermineInteractionTypeOfPathForCyclicNetwork <- function() {
    test_network <- igraph::graph_from_literal("node0" - +"node1", "node1" - +"node0")
    test_network <- igraph::set_edge_attr(test_network, "Weight", 1, 1)
    test_network <- igraph::set_edge_attr(test_network, "Weight", 2, -1)
    path <- igraph::shortest_paths(test_network, 1, 2, "out")
    # Fix for igraph 1.0.1
    if (!is.null(path$vpath)) {
        path <- lapply(path$vpath, as.vector)
    }
    interaction1 <- CausalR:::DetermineInteractionTypeOfPath(test_network, path)
    checkEquals(interaction1, 1)
    path2 <- igraph::shortest_paths(test_network, 2, 1, "out")
    # Fix for igraph 1.0.1
    if (!is.null(path2$vpath)) {
        path2 <- lapply(path2$vpath, as.vector)
    }
    interaction2 <- CausalR:::DetermineInteractionTypeOfPath(test_network, path2)
    checkEquals(interaction2, -1)
    
    # Change the order in which the edges are given, and consequently the edge numbers given to them by R
    test_network <- igraph::graph_from_literal("node1" - +"node0", "node0" - +"node1")
    test_network <- igraph::set_edge_attr(test_network, "Weight", 1, -1)
    test_network <- igraph::set_edge_attr(test_network, "Weight", 2, 1)
    path <- igraph::shortest_paths(test_network, 1, 2, "out")
    # Fix for igraph 1.0.1
    if (!is.null(path$vpath)) {
        path <- lapply(path$vpath, as.vector)
    }
    interaction3 <- CausalR:::DetermineInteractionTypeOfPath(test_network, path)
    checkEquals(interaction3, -1)
    path2 <- igraph::shortest_paths(test_network, 2, 1, "out")
    # Fix for igraph 1.0.1
    if (!is.null(path2$vpath)) {
        path2 <- lapply(path2$vpath, as.vector)
    }
    interaction4 <- CausalR:::DetermineInteractionTypeOfPath(test_network, path2)
    checkEquals(interaction4, 1)
}

test_GetMatrixOfCausalRelationshipsProducesTheCorrectOutput <- function() {
    test_network <- CreateSimpleNetworkForTest_ComputationalCausalGraphCreator()
    idsOfConnectedNodes <- c(1, 2, 3)
    matrixOfComputationalCausalRelationships <- CausalR:::GetMatrixOfCausalRelationships(1, test_network, idsOfConnectedNodes)
    checkEquals(typeof(matrixOfComputationalCausalRelationships), "double")
    checkEquals(length(matrixOfComputationalCausalRelationships), 3)
    checkEquals(matrixOfComputationalCausalRelationships[1], 1)
    checkEquals(matrixOfComputationalCausalRelationships[2], 1)
    checkEquals(matrixOfComputationalCausalRelationships[3], -1)
}

test_GetMatrixOfCausalRelationshipsWhenIDsOfNodesInNetworkDontMatchTheInternalReference <- function() {
    network <- igraph::graph_from_literal("node0" - +"node1", "node0" - +"node2")
    network <- igraph::set_edge_attr(network, "Weight", 1, 1)
    network <- igraph::set_edge_attr(network, "Weight", 2, -1)
    network <- igraph::set_vertex_attr(network, "ID", 1:3, c(17, 6, 54))
    idsOfConnectedNodes <- c(1, 2, 3)
    matrixOfComputationalCausalRelationships <- CausalR:::GetMatrixOfCausalRelationships(1, network, idsOfConnectedNodes)
    checkEquals(typeof(matrixOfComputationalCausalRelationships), "double")
    checkEquals(length(matrixOfComputationalCausalRelationships), 3)
    checkEquals(matrixOfComputationalCausalRelationships[1], 1)
    checkEquals(matrixOfComputationalCausalRelationships[2], 1)
    checkEquals(matrixOfComputationalCausalRelationships[3], -1)
}




test_FindIdsOfConnectedNodesInSubgraphWorksCorrectly <- function() {
    subgraph <- igraph::graph_from_literal("node0" - +"node1", "node0" - +"node2")
    subgraph <- igraph::set_edge_attr(subgraph, "Weight", 1, 1)
    subgraph <- igraph::set_edge_attr(subgraph, "Weight", 2, -1)
    subgraph <- igraph::set_vertex_attr(subgraph, "ID", 1:3, c(10, 19, 37))
    connectedNodes <- c(19, 37, 10)
    idsOfNodesInSubgraph <- CausalR:::FindIdsOfConnectedNodesInSubgraph(connectedNodes, subgraph)
    checkEquals(length(idsOfNodesInSubgraph), 3)
    checkEquals(idsOfNodesInSubgraph[1], 2)
    checkEquals(idsOfNodesInSubgraph[2], 3)
    checkEquals(idsOfNodesInSubgraph[3], 1)
}

test_MakePredictionsFromCCGProducesTheCorrectNodeListWithPositiveRegulation <- function() {
    test_double_network <- CausalR::CreateCCG(system.file("testData", "test_network.sif", package = "CausalR"))
    nodesInExperimentalData <- c(1, 2, 3)
    matrixOfCausalRelationships <- CausalR::MakePredictionsFromCCG(1, +1, test_double_network, 3, nodesInExperimentalData)
    checkEquals(typeof(matrixOfCausalRelationships), "double")
    checkEquals(dim(matrixOfCausalRelationships)[1], 3)
    checkEquals(dim(matrixOfCausalRelationships)[2], 2)
    checkEquals(matrixOfCausalRelationships[1, 1], 1)
    checkEquals(matrixOfCausalRelationships[2, 1], 2)
    checkEquals(matrixOfCausalRelationships[3, 1], 3)
    checkEquals(matrixOfCausalRelationships[1, 2], 1)
    checkEquals(matrixOfCausalRelationships[2, 2], 1)
    checkEquals(matrixOfCausalRelationships[3, 2], -1)
}


test_MakePredictionsFromCCGProducesTheCorrectNodeListWhenTheHypothesisNodeIsNotConnectedToAnyOther <- function() {
    # By 'not connected to any other', it is meant that this node has nodes following on from it in the directed graph
    test_double_network <- CausalR::CreateCCG(system.file("testData", "test_network.sif", package = "CausalR"))
    nodesInExperimentalData <- c(1, 2, 3)
    matrixOfCausalRelationships <- CausalR::MakePredictionsFromCCG(2, +1, test_double_network, 3, nodesInExperimentalData)
    checkEquals(typeof(matrixOfCausalRelationships), "double")
    checkEquals(dim(matrixOfCausalRelationships)[1], 1)
    checkEquals(dim(matrixOfCausalRelationships)[2], 2)
    checkEquals(matrixOfCausalRelationships[1, 1], 2)
    checkEquals(matrixOfCausalRelationships[1, 2], 1)
}

test_MakePredictionsFromCCGWorksWithBrokenLoops <- function() {
    test_double_network <- CausalR::CreateCG(system.file("testData", "network_with_loop.sif", package = "CausalR"))
    nodesInExperimentalData <- c(1, 2)
    # Network is 0 -> 1 -> 0. predictions should break the loop i.e. return 0 -> 1 even if asked to go to depth > 1.  First check that it works for
    # following one link:
    predictions <- CausalR::MakePredictionsFromCCG(1, +1, test_double_network, 1, nodesInExperimentalData)
    checkEquals(typeof(predictions), "double")
    checkEquals(ncol(predictions), 2)
    # Then check we get the same answer following two links:
    predictions <- CausalR::MakePredictionsFromCCG(1, +1, test_double_network, 2, nodesInExperimentalData)
    checkEquals(ncol(predictions), 2)
}


test_MakePredictionsFromCCGWorksOnANetworkWithMoreThanOneLevel <- function() {
    test_CCG_with_two_levels <- CausalR::CreateCCG(system.file("testData", "CCGWithTwoLevels.txt", package = "CausalR"))
    
    # Test when delta = 1 (signOfHypothesis = 1)
    matrixOfCausalRelationships <- CausalR::MakePredictionsFromCCG("node0", +1, test_CCG_with_two_levels, 1)
    checkEquals(typeof(matrixOfCausalRelationships), "double")
    checkEquals(dim(matrixOfCausalRelationships)[1], 3)
    checkEquals(dim(matrixOfCausalRelationships)[2], 2)
    checkEquals(matrixOfCausalRelationships[1, 1], 1)
    checkEquals(matrixOfCausalRelationships[2, 1], 2)
    checkEquals(matrixOfCausalRelationships[3, 1], 3)
    checkEquals(matrixOfCausalRelationships[1, 2], 1)
    checkEquals(matrixOfCausalRelationships[2, 2], 1)
    checkEquals(matrixOfCausalRelationships[3, 2], 1)
    
    # Test when delta = 2 (signOfHypothesis = -1)
    matrixOfCausalRelationships2 <- CausalR::MakePredictionsFromCCG("node0", -1, test_CCG_with_two_levels, 2)
    checkEquals(typeof(matrixOfCausalRelationships), "double")
    checkEquals(dim(matrixOfCausalRelationships2)[1], 6)
    checkEquals(dim(matrixOfCausalRelationships2)[2], 2)
    checkEquals(matrixOfCausalRelationships2[1, 1], 1)
    checkEquals(matrixOfCausalRelationships2[2, 1], 2)
    checkEquals(matrixOfCausalRelationships2[3, 1], 3)
    checkEquals(matrixOfCausalRelationships2[4, 1], 4)
    checkEquals(matrixOfCausalRelationships2[5, 1], 5)
    checkEquals(matrixOfCausalRelationships2[6, 1], 6)
    checkEquals(matrixOfCausalRelationships2[1, 2], -1)
    checkEquals(matrixOfCausalRelationships2[2, 2], -1)
    checkEquals(matrixOfCausalRelationships2[3, 2], -1)
    checkEquals(matrixOfCausalRelationships2[4, 2], 1)
    checkEquals(matrixOfCausalRelationships2[5, 2], 1)
    checkEquals(matrixOfCausalRelationships2[6, 2], -1)
} 
