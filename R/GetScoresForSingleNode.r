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
#' @param correctPredictionsThreshold A threshold on the number of correct predictions for a given hypothesis. If a hypothesis produces fewer correct predictions than predictionsThreshold then the algorithm will not calculate the two p-values. Instead 'NA' will be displayed in the final two columns of the corresponding row of the results table. As a default correctPredictionsThreshold is set as -Inf, so that the p-values are calculated for all specified hypotheses.
#' @param experimentalDataStats Stats from the experimental data
#' @param quiet a flag to supress progress output
#' @return If symmetricCCG is false, this returns a single line of the scoreMatrix for the 'iNode'th node in nodesToBeTested. If symmetricCCG is true this returns two lines. The first of which corresponds to the positive node and the second the negative node.

GetScoresForSingleNode <- function(iNode, timeToRunSoFar, nodesToBeTested, network, delta, processedExperimentalData, numPredictions, epsilon, useCubicAlgorithm,
                                   use1bAlgorithm, symmetricCCG, correctPredictionsThreshold, experimentalDataStats, quiet) {
    
    ## Set up a vector to hold scores that will go into a row of scoresMatrix in RankTheHypothesis
    scoresMatrixRow <- matrix(0, 1, 8)
    
    ## For this node get the predictions from the network within a given delta
    nodeID <- nodesToBeTested[iNode]
    
    scoresMatrixRow[1] <- nodeID
    
    ## Second column of scores matrix contains the value +1/-1 depending on whether we are testing the hypothesis node to be upregulated or downregulated.
    ## If not a symmetricCCG the second half of nodesToBeTested are downregulated
    if (!symmetricCCG && iNode > length(nodesToBeTested)/2) {
        scoresMatrixRow[2] <- -1
    } else {
        scoresMatrixRow[2] <- 1
    }
        
    prediction <- MakePredictions(nodeID, 1, network, delta, processedExperimentalData[, 1])
    
    if (!is.null(prediction)) {
        # Get the values of q+, q-, q0 and get scoreBreakdown by comparing prediction to data
        predictionListStats <- AnalysePredictionsList(prediction, numPredictions)
        scoreBreakdown <- ScoreHypothesis(prediction, processedExperimentalData)
        score <- scoreBreakdown[1]
        scoresMatrixRow[3:6] <- scoreBreakdown
        
        # Only calculate remaining values if the number of correct predictions is below correctPredictionsThreshold
        if (scoreBreakdown[2] > correctPredictionsThreshold) {
            scoresMatrixRow[7] <- CalculateSignificance(score, predictionListStats, experimentalDataStats, epsilon,
                                                        useCubicAlgorithm, use1bAlgorithm)
            scoresMatrixRow[8] <- CalculateEnrichmentPValue(prediction, processedExperimentalData)
        } else {
            # If there aren't enough correct predictions, don't calculate the p-values just report NA
            scoresMatrixRow[7] <- NA
            scoresMatrixRow[8] <- NA
        }
    } else {
        # No overlap between predictions and data, score will be 0 and this is the only score that can be achieved so P values should be 1
        scoresMatrixRow[7] <- 1
        scoresMatrixRow[8] <- 1
    }
    
    
    ## If the network is symmetric the second half of the network (downregulated nodes) can be obtained by symmetry without having to re-calculate
    ## using MakePredictions. So if symmetric CCg network add another row and fill it with the scores. 
    if (symmetricCCG) {
        scoresMatrixRow <- rbind(scoresMatrixRow, 0)
        
        scoresMatrixRow[2,1] <- nodeID
        scoresMatrixRow[2,2] <- -1
        
        # Second half of the table: downregulate the hypothesis node.  Predictions and scores are obtained by symmetry. The quantity
        # iNode+numNodesToBeTested is the iNode for the corresponding node in the CCG i.e. it would give the id of node1- for given the id of node1+
        if (!is.null(prediction)) {
            prediction[, 2] <- -prediction[, 2]
            scoresMatrixRow[2, 3] <- -score
            # For the opposite hypothesis the number of correct predictions becomes the number of incorrect and vice versa
            scoresMatrixRow[2, 4:6] <- scoreBreakdown[c(3, 2, 4)]
            # Check the number of correct predictions, in the case of a symmetric CCG this is just the number of incorrect predictions for the opposite
            # hypothesis
            if (scoreBreakdown[3] > correctPredictionsThreshold) {
                scoresMatrixRow[2, 7] <- CalculateSignificance(
                    -score, predictionListStats[c(2, 1, 3)], experimentalDataStats,
                    epsilon, useCubicAlgorithm, use1bAlgorithm
                )
                scoresMatrixRow[2, 8] <- CalculateEnrichmentPValue(prediction, processedExperimentalData)
            } else {
                # If there aren't enough correct predictions, don't calculate the p-values just report NA
                scoresMatrixRow[2, 7] <- NA
                scoresMatrixRow[2, 8] <- NA
            }
        } else {
            # No overlap between predictions and data, score will be 0 and this is the only score that can be achieved so P values should be 1
            scoresMatrixRow[2, 7] <- 1
            scoresMatrixRow[2, 8] <- 1
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