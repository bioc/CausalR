#' @title Get scores for single node
#' @description
#' A helper function for RankTheHypotheses to calculate a line of the scoresMatrix table
#' @param iNode this node
#' @param timeToRunSoFar the time to run so far
#' @param nodesToBeTested List of all nodes to be tested
#' @param network Computational Causal Graph, as an igraph.
#' @param delta Distance to search within the causal graph.
#' @param processedExperimentalData The processed experimental data
#' @param numPredictions The number of predictions
#' @param epsilon The threshold that is used when calculating the p-value using the cubic algorithm (see 'Assessing statistical significance in causal graphs').
#' @param useCubicAlgorithm An indicator specifying which algorithm will be used to calculate the p-value. The default is set as useCubicAlgorithm = TRUE which uses the cubic algorithm. If this value is set as FALSE, the algorithm will use the much slower quartic algorithm which does compute the exact answer, as opposed to using approximations like the cubic algorithm.
#' @param use1bAlgorithm An indicator specifying whether the 1a or 1b (default, faster) variant of the cubic algorithm described in Chindelevitch's paper will be used to calculate the p-value.
#' @param symmetricCCG This flag specifies whether the CCG is assumed to be symmetric. The value is set as TRUE as a default. If this is the case the running time of the algorithm is reduced since the negative node values can be calculated using symmetry and the results of calculations performed for the positive node
#' @param correctPredictionsThreshold A threshold on the number of correct predictions for a given hypothesis. If a hypothesis produces fewer correct predictions than predictionsThreshold then the algorithm will not calculate the two p-values. Instead 'NA' will be displayed in the final two columns of the corresponding row of the results table. As a default correctPredictionsThreshold is set as -Inf, so that the p-values are calculated for all specified hypotheses. Note: Set to Inf to turn off p-value calculations entirely.
#' @param experimentalDataStats Stats from the experimental data
#' @param quiet a flag to supress progress output
#' @return If symmetricCCG is false, this returns a single line of the scoreMatrix for the 'iNode'th node in nodesToBeTested. If symmetricCCG is true this returns two lines. The first of which corresponds to the positive node and the second the negative node.

GetScoresForSingleNode <- function(iNode, timeToRunSoFar, nodesToBeTested, network, delta, processedExperimentalData, numPredictions, epsilon, useCubicAlgorithm,
                                   use1bAlgorithm, symmetricCCG, correctPredictionsThreshold, experimentalDataStats, quiet) {
    
    ## Set up a matrix to hold rows that will be added to scoresMatrix in RankTheHypothesis
    if (symmetricCCG) {
        scoresMatrixRow <- matrix(0, 2, 8)
    } else {
        scoresMatrixRow <- matrix(0, 1, 8)
    }
    colnames(scoresMatrixRow) <- c("NodeID", "Regulation", "Score", "Correct", "Incorrect", "Ambiguous", "p-value", "Enrichment p-value")
    
    ## For this node get the predictions from the network within a given delta
    nodeID <- nodesToBeTested[iNode]
    
    scoresMatrixRow[1, "NodeID"] <- nodesToBeTested[iNode]
    
    ## Second column of scores matrix contains the value +1/-1 depending on whether we are testing the hypothesis node to be upregulated or downregulated.
    ## If not a symmetricCCG the second half of nodesToBeTested are downregulated
    if (!symmetricCCG && iNode > length(nodesToBeTested)/2) {
        scoresMatrixRow[1, "Regulation"] <- -1
    } else {
        scoresMatrixRow[1, "Regulation"] <- 1
    }
    
    prediction <- MakePredictions(hypothesisnode = scoresMatrixRow[1, "NodeID"], signOfHypothesis = 1,
                                  network = network, delta = delta, nodesInExperimentalData = processedExperimentalData[, 1])
    
    if (!is.null(prediction)) {
        # Get the values of q+, q-, q0 and get scoreBreakdown by comparing prediction to data
        predictionListStats <- AnalysePredictionsList(prediction, numPredictions)
        scoreBreakdown <- ScoreHypothesis(prediction, processedExperimentalData)

        scoresMatrixRow[1, c("Score", "Correct", "Incorrect", "Ambiguous")] <- scoreBreakdown
        
        # Only calculate remaining values if the number of correct predictions is below correctPredictionsThreshold
        if (scoresMatrixRow[1, "Correct"] > correctPredictionsThreshold) {
            scoresMatrixRow[1, "p-value"] <- CalculateSignificance(scoresMatrixRow[1, "Score"], predictionListStats, experimentalDataStats, epsilon, useCubicAlgorithm, use1bAlgorithm)
            scoresMatrixRow[1, "Enrichment p-value"] <- CalculateEnrichmentPValue(prediction, processedExperimentalData)
        } else {
            # If there aren't enough correct predictions, don't calculate the p-values just report NA
            scoresMatrixRow[1, "p-value"] <- NA
            scoresMatrixRow[1, "Enrichment p-value"] <- NA
        }
    } else {
        # No overlap between predictions and data, score will be 0 and this is the only score that can be achieved so P values should be 1
        scoresMatrixRow[1, "p-value"] <- 1
        scoresMatrixRow[1, "Enrichment p-value"] <- 1
    }
    
    
    ## If the network is symmetric the second half of the network (downregulated nodes) can be obtained by symmetry without having to re-calculate
    ## using MakePredictions. So if symmetric CCg network add another row and fill it with the scores. 
    if (symmetricCCG) {

        scoresMatrixRow[2, "NodeID"] <- scoresMatrixRow[1, "NodeID"]
        scoresMatrixRow[2, "Regulation"] <- -scoresMatrixRow[1, "Regulation"]
        
        # Second half of the table: downregulate the hypothesis node.  Predictions and scores are obtained by symmetry. The quantity
        # iNode+numNodesToBeTested is the iNode for the corresponding node in the CCG i.e. it would give the id of node1- for given the id of node1+
        if (!is.null(prediction)) {
            prediction[, 2] <- -prediction[, 2]
            scoresMatrixRow[2, "Score"] <- -scoresMatrixRow[1, "Score"]
            # For the opposite hypothesis the number of correct predictions becomes the number of incorrect and vice versa
            scoresMatrixRow[2, c("Correct", "Incorrect", "Ambiguous")] <- scoresMatrixRow[1, c("Incorrect", "Correct", "Ambiguous")]
            
            # Check the number of correct predictions, in the case of a symmetric CCG this is just the number of incorrect predictions for the opposite
            # hypothesis
            if (scoresMatrixRow[2, "Correct"] > correctPredictionsThreshold) {
                scoresMatrixRow[2, "p-value"] <- CalculateSignificance(scoresMatrixRow[2, "Score"], predictionListStats[c(2, 1, 3)], experimentalDataStats,
                    epsilon, useCubicAlgorithm, use1bAlgorithm
                )
                scoresMatrixRow[2, "Enrichment p-value"] <- CalculateEnrichmentPValue(prediction, processedExperimentalData)
            } else {
                # If there aren't enough correct predictions, don't calculate the p-values just report NA
                scoresMatrixRow[2, "p-value"] <- NA
                scoresMatrixRow[2, "Enrichment p-value"] <- NA
            }
        } else {
            # No overlap between predictions and data, score will be 0 and this is the only score that can be achieved so P values should be 1
            scoresMatrixRow[2, "p-value"] <- 1
            scoresMatrixRow[2, "Enrichment p-value"] <- 1
        }
    }
    
    if (!quiet && iNode %% 100 == 0) {
        #cat(paste0("Node Reached: ", iNode, ". "))
        #timeToRunNextHundred <- proc.time()[1]
        #cat(paste("Time to run this hundred nodes:", (timeToRunNextHundred - timeToRunSoFar), "s\n"))
        timeDiff <- Sys.time() - timeToRunSoFar
        cat(paste0("Time taken to reach Node ", iNode, ": ", round(timeDiff, 2), " ", units(timeDiff), "\n"))
        
        #timeToRunSoFar <- timeToRunNextHundred
    }
    
    return(scoresMatrixRow)
}