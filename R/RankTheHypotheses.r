#' @title rank the hypotheses
#' @description 
#' Rank the hypotheses in the causal network
#' @export
#' @param network Computational Causal Graph, as an igraph.
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. The results is an n x 2 matrix; where the first column contains the node ids of the nodes in the network that the results refer to. The second column contains values indicating the direction of regulation in the results - (+)1 for up, -1 for down and 0 for insignificant amounts of regulation.
#' @param delta Distance to search within the causal graph.
#' @param epsilon The threshold that is used when calculating the p-value using the cubic algorithm (see "Assessing statistical significance in causal graphs").
#' @param useCubicAlgorithm An indicator specifying which algorithm will be used to calculate the p-value. The default is set as useCubicAlgorithm = TRUE which uses the cubic algorithm. If this value is set as FALSE, the algorithm will use the much slower quartic algorithm which does compute the exact answer, as opposed to using approximations like the cubic algorithm.
#' @param use1bAlgorithm An indicator specifying whether the 1a or 1b (default, faster) variant of the cubic algorithm described in Chindelevitch's paper will be used to calculate the p-value.
#' @param symmetricCCG This flag specifies whether the CCG is assumed to be symmetric. The value is set as TRUE as a default. If this is the case the running time of the algorithm is reduced since the bottom half of the table can be filled in using the results of calculations performed earlier.
#' @param listOfNodes A list of nodes specified by the user. The algorithm will only calculate and store the results for the nodes in the specified list. The default value is NULL; here the algorithm will calculate and store results for all the nodes in the network.
#' @param correctPredictionsThreshold A threshold on the number of correct predictions for a given hypothesis. If a hypothesis produces fewer correct predictions than predictionsThreshold then the algorithm will not calculate the two p-values. Instead "NA" will be displayed in the final two columns of the corresponding row of the results table. As a default correctPredictionsThreshold is set as -Inf, so that the p-values are calculated for all specified hypotheses.
#' @return A data frame containing the results of the algorithm.
#' @examples
#' #get path to example network file
#' network <- system.file(package="CausalR", "extdata", "testNetwork.sif")
#' #create ccg
#' ccg <- CreateCCG(network)
#' #get path to example experimental data
#' fileName<- system.file(package="CausalR", "extdata", "testData.txt")
#' #read in experimetal data
#' expData<- ReadExperimentalData(fileName, ccg)

#' RankTheHypotheses(ccg, expData, 2)


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.


RankTheHypotheses <- function(network, experimentalData, delta, epsilon = 1e-5, useCubicAlgorithm = TRUE, use1bAlgorithm = TRUE, symmetricCCG = TRUE, listOfNodes = NULL, correctPredictionsThreshold = -Inf){
  
  # Set up timer
  timeToRunSoFar <- proc.time()[1]
  
  if (is.numeric(experimentalData[1,1])){
    # experimentalData is already processed (i.e. has node IDs rather than text node names)
    processedExperimentalData <- experimentalData
  }
  else{
    processedExperimentalData <- ProcessExperimentalData(experimentalData, network)
  }
  
  isCCG <- network$isCCG
  if (is.null(isCCG)){
    # Network doesn't specify whether it's a CCG or not - assume it's a CG
    isCCG <- FALSE
  }
  
  if (isCCG){
    numNodes <- vcount(network)/2
  }
  else{
    numNodes <- vcount(network)
  }
  
  # If listOfNodes is null test all the hypotheses, otherwise only test the hypotheses in the list
  if (is.null(listOfNodes)){
    numNodesToBeTested <- numNodes
    nodesToBeTested <- c(1:numNodesToBeTested)
  }
  else{
    numNodesToBeTested <- length(listOfNodes)
    # If listOfNodes is a character array convert to nodeIDs
    if (typeof(listOfNodes) == "character"){
      nodesToBeTested <- GetNodeID(network, listOfNodes)
    }
    else{
      nodesToBeTested <- listOfNodes
    }
  }
  cat(paste("Number of Nodes to analyse:", numNodesToBeTested,"\n"))
  
  numPredictions <- nrow(processedExperimentalData)
  # Get the values of n+, n- and n0
  experimentalDataStats <- AnalyseExperimentalData(processedExperimentalData)
  
  # Create an empty matrix to store results of scoring
  scoresMatrix <- matrix(0,2*numNodesToBeTested,8)
  
  # The second column will contain the value +1/-1 depending on whether we
  # are testing the hypothesis node to be upregulated or downregulated
  # The first half of the table are upregulated and the second half are downregulated
  scoresMatrix[1:numNodesToBeTested,2] <- +1
  scoresMatrix[(numNodesToBeTested+1):(2*numNodesToBeTested),2] <- -1
  
  if (!symmetricCCG){
    # Add the downregulated nodes on to the list of hypotheses to test
    nodesToBeTested <- c(nodesToBeTested,nodesToBeTested+numNodes)
    numNodesToBeTested <- numNodesToBeTested * 2
    scoresMatrix[,1] <- nodesToBeTested
    nodeNames <- V(network)$name[nodesToBeTested]
    unsignedNodeNames <- V(network)$unsignedName[nodesToBeTested]
  }
  else{
    scoresMatrix[1:numNodesToBeTested,1] <- nodesToBeTested
    scoresMatrix[(numNodesToBeTested+1):(2*numNodesToBeTested),1] <- scoresMatrix[1:numNodesToBeTested,1]
    nodeNames <- c(paste(GetNodeName(network, nodesToBeTested),'+',sep=''),paste(GetNodeName(network, nodesToBeTested),'-',sep=''))
    unsignedNodeNames <- c(GetNodeName(network, nodesToBeTested),GetNodeName(network, nodesToBeTested))
  }
  
  for (iNode in 1:numNodesToBeTested){
    nodeID <- nodesToBeTested[iNode]
    # First half of the table: upregulate the hypothesis node
    prediction <- MakePredictions(nodeID, 1, network, delta, processedExperimentalData[,1])
    
    if (!is.null(prediction)){
      # Get the values of q+, q-, q0
      predictionListStats <- AnalysePredictionsList(prediction,numPredictions)
      scoreBreakdown <- ScoreHypothesis(prediction, processedExperimentalData)
      score <- scoreBreakdown[1]
      scoresMatrix[iNode,3:6] <- scoreBreakdown
      # Only calculate remaining values if the number of correct predictions is below correctPredictionsThreshold
      if (scoreBreakdown[2] > correctPredictionsThreshold){
        scoresMatrix[iNode,7] <- CalculateSignificance(score, predictionListStats, experimentalDataStats, epsilon, useCubicAlgorithm, use1bAlgorithm)
        scoresMatrix[iNode,8] <- CalculateEnrichmentPValue(prediction, processedExperimentalData)
      }
      else{
        # If there aren't enough correct predictions, don't calculate the p-values just report NA
        scoresMatrix[iNode,7] <- NA
        scoresMatrix[iNode,8] <- NA
      }
    }
    else{
      # No overlap between predictions and data, score will be 0 and this is the only score that can be achieved so P values should be 1
      scoresMatrix[iNode,7] <- 1
      scoresMatrix[iNode,8] <- 1
    }
    
    if (symmetricCCG){
      # Second half of the table: downregulate the hypothesis node.
      # Predictions and scores are obtained by symmetry. The quantity iNode+numNodesToBeTested is the iNode for the corresponding node in the CCG
      # i.e. it would give the id of node1- for given the id of node1+
      if (!is.null(prediction)){
        prediction[,2] <- -prediction[,2]
        scoresMatrix[numNodesToBeTested+iNode,3] <- -score			
        # For the opposite hypothesis the number of correct predictions becomes the number of incorrect and vice versa
        scoresMatrix[numNodesToBeTested+iNode,4:6] <- scoreBreakdown[c(3,2,4)]
        # Check the number of correct predictions, in the case of a symmetric CCG this is just the number of incorrect predictions for the opposite hypothesis
        if (scoreBreakdown[3] > correctPredictionsThreshold){
          scoresMatrix[numNodesToBeTested+iNode,7] <- CalculateSignificance(-score, predictionListStats[c(2,1,3)], experimentalDataStats, epsilon, useCubicAlgorithm, use1bAlgorithm)
          scoresMatrix[numNodesToBeTested+iNode,8] <- CalculateEnrichmentPValue(prediction, processedExperimentalData)
        }
        else{
          # If there aren't enough correct predictions, don't calculate the p-values just report NA
          scoresMatrix[numNodesToBeTested+iNode,7] <- NA
          scoresMatrix[numNodesToBeTested+iNode,8] <- NA
        }
      }
      else{
        # No overlap between predictions and data, score will be 0 and this is the only score that can be achieved so P values should be 1
        scoresMatrix[numNodesToBeTested+iNode,7] <- 1
        scoresMatrix[numNodesToBeTested+iNode,8] <- 1
      }
    }
    
    if (iNode %% 100 == 0){
      cat(paste("Node Reached:", iNode))
      timeToRunNextHundred <- proc.time()[1]
      cat(paste("Time to run this hundred nodes:",(timeToRunNextHundred - timeToRunSoFar), "s\n"))
      timeToRunSoFar <- timeToRunNextHundred
    }
  }
  
  # Sort the output table in score order
  orderHypotheses <- order(scoresMatrix[,3],decreasing=TRUE)
  rankedHypotheses <- data.frame(scoresMatrix, row.names=nodeNames)
  colnames(rankedHypotheses) <- c("NodeID", "Regulation", "Score", "Correct", "Incorrect", "Ambiguous", "p-value", "Enrichment p-value")
  rankedHypotheses <- rankedHypotheses[orderHypotheses,]
  
  # Save results to an text file
  rhoutput <- scoresMatrix[,c(1,1:8)]
  rhoutput[,1] <- unsignedNodeNames
  rhoutput <- rhoutput[orderHypotheses,]
  colnames(rhoutput) <- c("NodeName", "NodeID", "Regulation", "Score", "Correct", "Incorrect", "Ambiguous", "p-value", "Enrichment p-value")
  write.table(rhoutput, paste(getwd(), "/ResultsTable.txt", sep =""), sep="\t", row.names = FALSE, quote=FALSE)
  
  return(rankedHypotheses)
}
