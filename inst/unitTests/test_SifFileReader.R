# Copyright: Copyright 2012 GSK. All rights reserved *** SVN *** LastChanged: $Date: 2016-09-21 17:28:25 +0100 (Wed, 21 Sep 2016) $ Changed By:
# $Author: pipm $ Version: $Revision: 636 $ Source: $HeadURL: https://stevenagefs:7777/7412/trunk/NetworkFileReader/test_SifFileReader.R $ ***
# Summary *** Description: A test function for the CreateCG and CreateCCG functions

# Create a table to be used in the testsuite for CreateCG
CreateTestTableForTest_SifFileReader <- function() {
    matrix1 <- matrix(c("node0", "node0", "Activates", "Inhibits", "node1", "node2"), nrow = 2)
    tab <- as.table(matrix1)
    
    return(tab)
}

# Create a network for use in the testsuite for CreateCG
CreateNetworkForTest_SifFileReader <- function() {
    network <- igraph::graph_from_literal("node0" - +"node1", "node0" - +"node2")
    
    return(network)
}

# Create interactionInfo table for use in the testsuite for CreateCG
CreateInteractionInfoTableForTest_SifFileReader <- function() {
    matrix1 <- matrix(c("Activates", "Inhibits"))
    tab <- as.table(matrix1)
    
    return(tab)
}

# A helper file for the function Test_SifFileReader
CheckEdgeForTest_SifFileReader <- function(edge, vertex1, vertex2) {
    checkEquals(edge[1], vertex1) && checkEquals(edge[2], vertex2)
}

test_ReadSifFileToTableReadsInformationCorrectly <- function() {
    tableOfConnections <- CausalR:::ReadSifFileToTable(system.file("testData", "test_network.sif", package = "CausalR"))
    checkEquals(dim(tableOfConnections)[1], 2)
    checkEquals(dim(tableOfConnections)[2], 3)
    checkEquals(tableOfConnections[1, 1], "node0")
    checkEquals(tableOfConnections[1, 1], "node0")
    checkEquals(tableOfConnections[1, 2], "Activates")
    checkEquals(tableOfConnections[1, 3], "node1")
    checkEquals(tableOfConnections[2, 1], "node0")
    checkEquals(tableOfConnections[2, 2], "Inhibits")
    checkEquals(tableOfConnections[2, 3], "node2")
}

test_CreateNetworkFromTableProducesCorrectNetwork <- function() {
    tableOfConnections <- CreateTestTableForTest_SifFileReader()
    test_network <- CausalR:::CreateNetworkFromTable(tableOfConnections)
    networkEdges = igraph::E(test_network)
    networkEdgeList = igraph::as_edgelist(test_network)
    checkEquals(dim(networkEdgeList)[1], 2) && checkEquals(dim(networkEdgeList)[2], 2)
    # Check node 0 is connected to node 1
    CheckEdgeForTest_SifFileReader(networkEdgeList[1, ], "node0", "node1")
    # Check node 0 is connected to node 2
    CheckEdgeForTest_SifFileReader(networkEdgeList[2, ], "node0", "node2")
    # Check incorrect request fails
    checkTrue(!(networkEdgeList[1, 2] == "node3" && networkEdgeList[1, 1] == "node0"))
}

test_GetInteractionInformationProducesCorrectTable <- function() {
    tableOfConnections <- CreateTestTableForTest_SifFileReader()
    interactionInfo <- CausalR:::GetInteractionInformation(tableOfConnections)
    checkEquals(interactionInfo[[1]], "Activates")
    checkEquals(interactionInfo[[2]], "Inhibits")
    checkEquals(length(interactionInfo), 2)
}

test_GetWeightsFromInteractionInformation <- function() {
    interactionInfo <- c("Activates", "Inhibits", "Activates", "Activates", "Inhibits")
    weightInformation <- CausalR:::GetWeightsFromInteractionInformation(interactionInfo)
    checkEquals(nrow(weightInformation), 5)
    checkEquals(ncol(weightInformation), 1)
    checkEquals(weightInformation[1], 1)
    checkEquals(weightInformation[2], -1)
    checkEquals(weightInformation[3], 1)
    checkEquals(weightInformation[4], 1)
    checkEquals(weightInformation[5], -1)
}

test_AddEdgeInformationAssignsCorrectWeights <- function() {
    tableOfConnections <- CreateTestTableForTest_SifFileReader()
    test_network <- CreateNetworkForTest_SifFileReader()
    interactionInformation <- CreateInteractionInfoTableForTest_SifFileReader()
    test_network <- CausalR:::AddWeightsToEdges(test_network, interactionInformation)
    edge1Weight = igraph::edge_attr(test_network, "Weight", 1)
    checkEquals(edge1Weight, 1)
    edge2Weight = igraph::edge_attr(test_network, "Weight", 2)
    checkEquals(edge2Weight, -1)
}

test_AddIDsToVerticesWorksCorrectly <- function() {
    network <- igraph::graph_from_literal("node0" - +"node1", "node1" - +"node3", "node1" - +"node2")
    # To make the test realistic the edge attributes are added to the network
    network <- igraph::set_edge_attr(network, "Weight", 1, 1)
    network <- igraph::set_edge_attr(network, "Weight", 2, -1)
    network <- igraph::set_edge_attr(network, "Weight", 3, 1)
    network <- CausalR:::AddIDsToVertices(network)
    checkEquals(igraph::vertex_attr(network, "ID", "node0"), 1)
    checkEquals(igraph::vertex_attr(network, "ID", "node1"), 2)
    checkEquals(igraph::vertex_attr(network, "ID", "node3"), 3)
    checkEquals(igraph::vertex_attr(network, "ID", "node2"), 4)
}

test_ReadSifFileFailsWhenGivenANonsenseFile <- function() {
    checkException(CausalR::CreateCG("nonExistentFile.sif"))
}

test_ReadSifFileCreatesTheCorrectNetwork <- function() {
    network <- CausalR::CreateCG(system.file("testData", "test_network.sif", package = "CausalR"))
    checkEquals(igraph::gorder(network), 3)
    checkEquals(igraph::gsize(network), 2)
    checkEquals(igraph::vertex_attr(network, "ID", "node0"), 1)
    checkEquals(igraph::vertex_attr(network, "ID", "node1"), 2)
    checkEquals(igraph::vertex_attr(network, "ID", "node2"), 3)
    checkEquals(igraph::edge_attr(network, "Weight", 1), 1)
    checkEquals(igraph::edge_attr(network, "Weight", 2), -1)
}

test_CreateDoubleNetworkWorksCorrectly. <- function() {
    network <- CausalR::CreateCCG(system.file("testData", "test_network.sif", package = "CausalR"))
    checkEquals(igraph::gorder(network), 6)
    checkEquals(igraph::gsize(network), 4)
    checkEquals(igraph::V(network)[1]$name, "node0+")
    checkEquals(igraph::V(network)[2]$name, "node1+")
    checkEquals(igraph::V(network)[3]$name, "node2+")
    checkEquals(igraph::V(network)[4]$name, "node0-")
    checkEquals(igraph::V(network)[5]$name, "node1-")
    checkEquals(igraph::V(network)[6]$name, "node2-")
}

test_CreateDoubleNetworkWorksCorrectlyWithInclusionListAndExcludeTrue <- function() {
    network <- CausalR::CreateCCG(system.file("testData", "test_network.sif", package = "CausalR"),
                                  system.file("unitTests", "test_nodeInclusionFile.txt", package = "CausalR"),
                                  TRUE)
    checkEquals(igraph::gorder(network), 4)
    checkEquals(igraph::gsize(network), 2)
    checkEquals(igraph::V(network)[1]$name, "node0+")
    checkEquals(igraph::V(network)[2]$name, "node2+")
    checkEquals(igraph::V(network)[3]$name, "node0-")
    checkEquals(igraph::V(network)[4]$name, "node2-")
}

test_CreateDoubleNetworkWorksCorrectlyWithInclusionListAndExcludeFalse <- function() {
    network <- CausalR::CreateCCG(system.file("testData", "test_network.sif", package = "CausalR"),
                                  system.file("unitTests", "test_nodeInclusionFile.txt", package = "CausalR"),
                                  FALSE)
    checkEquals(igraph::gorder(network), 4)
    checkEquals(igraph::gsize(network), 2)
    checkEquals(igraph::V(network)[1]$name, "node0+")
    checkEquals(igraph::V(network)[2]$name, "node1+")
    checkEquals(igraph::V(network)[3]$name, "node0-")
    checkEquals(igraph::V(network)[4]$name, "node1-")
}


test_CreateDoubleNetworkFailsWhenGivenANonsenseFile <- function() {
    checkException(CausalR::CreateCCG("nonExistentFile.sif"))
} 
