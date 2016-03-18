#' @title compare hypothesis
#' @description
#' Compare the predictions from a hypothesis with the experimental data returning an matrix with columns for node ID, predictions, experimental results and the corresponding scores.
#' @export
#' @concept CausalR
#' @param matrixOfPredictions a matrix of predictions
#' @param matrixOfExperimentalData a matrix of experimental data
#' @param ccg a CCG network (default=NULL) 
#' @param sourceNode A starting node (default=NULL)
#' @return a matrix containing predictions, observations and scores.
#' @examples
#' predictions <- matrix(c(1,2,3,+1,0,-1),ncol=2)
#' experimentalData <- matrix(c(1,2,4,+1,+1,-1),ncol=2)
#' ScoreHypothesis(predictions,experimentalData)
#' CompareHypothesis(predictions,experimentalData)


CompareHypothesis <- function(matrixOfPredictions, matrixOfExperimentalData, ccg = NULL, sourceNode = NULL) {
    
    intersectnodes <- intersect(matrixOfPredictions[, 1], matrixOfExperimentalData[, 1])
    
    n <- length(intersectnodes)
    
    comparison <- data.frame(matrix(0, n, 4))
    colnames(comparison) <- c("NodeID", "Prediction", "Experiment", "Score")
    
    if (n > 0) {
        for (i in 1:n) {
            comparison[i, 1] <- intersectnodes[i]
            comparison[i, 2] <- matrixOfPredictions[which(matrixOfPredictions[, 1] == intersectnodes[i]), 2]
            comparison[i, 3] <- matrixOfExperimentalData[which(matrixOfExperimentalData[, 1] == intersectnodes[i]), 2]
        }
        
        comparison[, 4] <- comparison[, 2] * comparison[, 3]
    }
    
    if (!is.null(ccg)) {
        rownames(comparison) <- GetNodeName(ccg, comparison[, 1])
    }
    
    # Sort the outputs into order - correct, then 0, then wrong, and within each of these sort by experimental value then predicted value
    comparison <- comparison[order(comparison[, 4], comparison[, 3], comparison[, 2], decreasing = TRUE), ]
    
    # Save results to an text file - first create the filename
    if (is.null(sourceNode) || (is.numeric(sourceNode) && is.null(ccg))) {
        filename <- paste(getwd(), "/explained_Nodes.txt", sep = "")
    } else {
        if (is.numeric(sourceNode)) {
            filename <- paste(getwd(), "/", GetNodeName(ccg, sourceNode, signed = TRUE), "_explained_Nodes.txt", sep = "")
        } else {
            filename <- paste(getwd(), "/", sourceNode, "_explained_Nodes.txt", sep = "")
        }
    }
    
    # Format the data for saving, and save it.
    if (is.null(ccg)) {
        rhoutput <- data.frame(comparison)
        colnames(rhoutput) <- c("NodeID", "Prediction", "Experiment", "Score")
        utils::write.table(rhoutput, filename, sep = "\t", row.names = FALSE, quote = FALSE)
    } else {
        rhoutput <- data.frame(comparison[, c(1, 1:4)])
        rhoutput[, 1] <- rownames(comparison)
        colnames(rhoutput) <- c("NodeName", "NodeID", "Prediction", "Experiment", "Score")
        utils::write.table(rhoutput, filename, sep = "\t", row.names = FALSE, quote = FALSE)
    }
    
    return(comparison)
} 
