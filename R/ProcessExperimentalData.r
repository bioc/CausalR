#' @title process experimental data
#' @description
#' Processes experimental data to get it into the correct form
#' for scoring. The node names that are read in as strings acquire 
#' an internal id when the network is created. This function will 
#' replace the node name with its id.

#' @param experimentalData input experimental data.
#' @param network an input interaction network.
#' @return processed experimental data formatted ready for scoring

ProcessExperimentalData <- function(experimentalData, network) {
    
    numNodesInExperimentalData <- nrow(experimentalData)
    
    # A matrix of zeros that will ultimately store the experimental data results as an array containing purely doubles
    processedResults <- matrix(0, numNodesInExperimentalData, 2)
    # Populate the second column (the expression values)
    processedResults[, 1] <- GetNodeID(network, experimentalData[, 1])
    processedResults[, 2] <- as.numeric(experimentalData[, 2])
    
    # If any of the node names aren't in the network, output a warning message and delete those rows from the table.
    if (any(is.na(processedResults[, 1]))) {
        for (i in 1:numNodesInExperimentalData) {
            if (is.na(processedResults[i, 1])) {
                warning("Node in experimental data not found in the network: ", experimentalData[i, 1])
            }
        }
        processedResults <- processedResults[!is.na(processedResults[, 1]), ]
    }
    
    return(processedResults)
} 
