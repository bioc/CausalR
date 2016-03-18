#' @title Get explained nodes of CCG
#' @description
#' Returns a table of node names and values for explained nodes, I.e. nodes that appear in both network and data with the same sign.
#' The table contain the name in column 1 and the value (1 or -1) in column 2

#' @param hypothesisnode a hypothesis node
#' @param signOfHypothesis the direction of change of hypothesis node
#' @param network a computational causal graph
#' @param data a data file
#' @param delta the number of edges across which the hypothesis should be followed
#' @return vector of explained nodes

GetExplainedNodesOfCCG <- function(hypothesisnode, signOfHypothesis, network, data, delta) {
    experimentalData <- ReadExperimentalData(data, network)
    predictions <- MakePredictionsFromCCG(hypothesisnode, signOfHypothesis, network, delta)
    experimentalDataByName <- GetNodeName(network, experimentalData)
    predictionsByName <- GetNodeName(network, predictions)
    
    ## Merge the experimental data and prediction data tables to create a table containing only the
    ## nodes that are contained in both the experiment and prediction data
    tableOfMatches <- merge.default(experimentalDataByName, predictionsByName, by = c(TRUE, FALSE))
    
    # Tidy up tableOfMatches - remove factors and convert number holding strings to integers
    i <- sapply(tableOfMatches, is.factor)
    tableOfMatches[i] <- lapply(tableOfMatches[i], as.character)
    tableOfMatches[,2] <- as.integer(tableOfMatches[,2])
    tableOfMatches[,3] <- as.integer(tableOfMatches[,3])
    
    ## Explained nodes are those where the experimental value matches the prediction.
    ## Return a list of explained nodes with column 1 containing the names of the nodes
    ## and column 2 containing the value (+1 or -1)
    explainedNodes <- tableOfMatches[tableOfMatches[,2]==tableOfMatches[,3],1:2]
    
    return(explainedNodes)
}