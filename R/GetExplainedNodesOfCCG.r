#' @title Get explained nodes of CCG
#' @description
#' Returns a table of node names and values for explained nodes, I.e. nodes that appear in both network and data with the same sign.
#' The table contain the name in column 1 and the value (1 or -1) in column 2

#' @param hypothesisnode a hypothesis node
#' @param signOfHypothesis the direction of change of hypothesis node
#' @param network a computational causal graph
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. The results is an n x 2 matrix; where the first column contains the node ids of the nodes in the network that the results refer to. The second column contains values indicating the direction of regulation in the results - (+)1 for up, -1 for down and 0 for insignificant amounts of regulation. The name of the first column is the filename the data was read from.
#' @param delta the number of edges across which the hypothesis should be followed
#' @return vector of explained nodes

GetExplainedNodesOfCCG <- function(hypothesisnode, signOfHypothesis, network, experimentalData, delta) {
    
    signOfHypothesis <- as.integer(signOfHypothesis)
    delta <- as.integer(delta)
    
    predictions <- MakePredictionsFromCCG(hypothesisnode, signOfHypothesis, network, delta)
    experimentalDataByName <- GetNodeName(network, experimentalData)
    predictionsByName <- GetNodeName(network, predictions)
    
    # Set the column names - needed before merge
    colnames(experimentalDataByName) <- c("name", "exp")
    colnames(predictionsByName) <- c("name", "pred")
    
    ## Merge the experimental data and prediction data tables by first (name) column to create a
    ## table containing all nodes from both the experimental and prediction data
    tableOfMatches <- merge.default(experimentalDataByName, predictionsByName, all=TRUE, by = c(TRUE, FALSE))

    # Tidy up tableOfMatches - convert from factors to strings then to integers
    i <- sapply(tableOfMatches, is.factor)
    tableOfMatches[i] <- lapply(tableOfMatches[i], as.character)
    tableOfMatches[,"name"] <- as.character(tableOfMatches[,"name"])
    tableOfMatches[,"exp"] <- as.integer(tableOfMatches[,"exp"])
    tableOfMatches[,"pred"] <- as.integer(tableOfMatches[,"pred"])

    # Filter to only nodes that appear in both experimental and predictions
    tableOfMatches <- tableOfMatches[apply(tableOfMatches, 1, function(x) !any(is.na(x))),]
    
    # Get nodes that have the same regulation value for experimental and predictions
    corExplainedNodes <- tableOfMatches[tableOfMatches[,"exp"]*tableOfMatches[,"pred"] == 1,]
    #corExplainedNodes <- corExplainedNodes[corExplainedNodes[,"name"] != hypothesisnode,]
    
    # Get nodes that have different and non-zero values for experimental and predictions
    incorExplainedNodes <- tableOfMatches[tableOfMatches[,"exp"]*tableOfMatches[,"pred"] == -1,]
    
    # Get nodes that are in experimental but not in predictions 
    ambExplainedNodes <- tableOfMatches[tableOfMatches[,"pred"] == 0 & tableOfMatches[,"exp"] != 0,]
    # ambExplainedNodes <- tableOfMatches[apply(tableOfMatches, 1, function(x) any(is.na(x))),]
    
    
    ## In table of matches column 1 contains the name, column 2 contains experimental data and
    ## column 3 contains predictions data.
    ## Explained nodes are those where the experimental value matches the prediction.
    ## Get a table of explained nodes with column 1 containing the names of the nodes
    ## and column 2 containing the value (+1 or -1)
    #explainedNodes <- tableOfMatches[tableOfMatches[,2]==tableOfMatches[,3],1:2]
    
    explainedNodes <- list(corExplainedNodes = corExplainedNodes, incorExplainedNodes = incorExplainedNodes, ambExplainedNodes = ambExplainedNodes)
    
    return(explainedNodes)
}