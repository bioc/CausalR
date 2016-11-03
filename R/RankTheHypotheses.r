#' @title rank the hypotheses
#' @description
#' Rank the hypotheses in the causal network. This function can be run with parallelisation using the doParallel flag.
#' @export
#' @concept CausalR
#' @param network Computational Causal Graph, as an igraph.
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. The results is an n x 2 matrix; where the first column contains the node ids of the nodes in the network that the results refer to. The second column contains values indicating the direction of regulation in the results - (+)1 for up, -1 for down and 0 for insignificant amounts of regulation. The name of the first column is the filename the data was read from.
#' @param delta Distance to search within the causal graph.
#' @param epsilon The threshold that is used when calculating the p-value using the cubic algorithm (see 'Assessing statistical significance in causal graphs').
#' @param useCubicAlgorithm An indicator specifying which algorithm will be used to calculate the p-value. The default is set as useCubicAlgorithm = TRUE which uses the cubic algorithm. If this value is set as FALSE, the algorithm will use the much slower quartic algorithm which does compute the exact answer, as opposed to using approximations like the cubic algorithm.
#' @param use1bAlgorithm An indicator specifying whether the 1a or 1b (default, faster) variant of the cubic algorithm described in Chindelevitch's paper will be used to calculate the p-value.
#' @param symmetricCCG This flag specifies whether the CCG is assumed to be symmetric. The value is set as TRUE as a default. If this is the case the running time of the algorithm is reduced since the bottom half of the table can be filled in using the results of calculations performed earlier.
#' @param listOfNodes A list of nodes specified by the user. The algorithm will only calculate and store the results for the nodes in the specified list. The default value is NULL; here the algorithm will calculate and store results for all the nodes in the network.
#' @param correctPredictionsThreshold A threshold on the number of correct predictions for a given hypothesis. If a hypothesis produces fewer correct predictions than predictionsThreshold then the algorithm will not calculate the two p-values. Instead 'NA' will be displayed in the final two columns of the corresponding row of the results table. As a default correctPredictionsThreshold is set as -Inf, so that the p-values are calculated for all specified hypotheses.
#' @param quiet a flag to suppress output to console. FALSE by default.
#' @param doParallel A flag for running RankTheHypothesis in parallel mode.
#' @param numCores Number of cores to use if using parallel mode. If the default value of NULL is used, it will attempt to detect the number of cores available and use all of them bar one.
#' @param writeFile A flag for determining if the output should be written to a file in the working directory. Default is TRUE.
#' @param outputDir the directory to output the files to. Default is the working directory
#' @return A data frame containing the results of the algorithm.
#' @examples
#' #get path to example network file
#' networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' #create ccg
#' network <- CreateCCG(networkFile)
#' #get path to example experimental data
#' experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData.txt')
#' #read in experimetal data
#' experimentalData <- ReadExperimentalData(experimentalDataFile, network)
#' #run in single threaded mode
#' RankTheHypotheses(network, experimentalData, 2)
#' #run in parallel mode
#' RankTheHypotheses(network, experimentalData, 2, doParallel=TRUE, numCores=2)

#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.

RankTheHypotheses <- function(network, experimentalData, delta, epsilon = 1e-05, useCubicAlgorithm = TRUE, use1bAlgorithm = TRUE, symmetricCCG = TRUE,
                              listOfNodes = NULL, correctPredictionsThreshold = -Inf, quiet = FALSE, doParallel = FALSE, numCores = NULL, writeFile = TRUE, outputDir = getwd()) {
    
    delta <- as.integer(delta)
    
    # If outputDir doesn't exist, create it
    if (!file.exists(file.path(outputDir))){
        dir.create(file.path(outputDir))
    }
    
    # If CreateCCG was used to create 'network' it should have a name attribute containing the filename used to produce it.
    # If ReadExperimentalData was used to create 'experimentalData' the name of the first column should be equal to the filename used to produce it
    fileOutputName <- paste0(network$name, "-", colnames(experimentalData)[1], "-delta", delta)
    
    # Set up timer
        timeToRunSoFar <- Sys.time()
        
        if (is.numeric(experimentalData[1, 1])) {
            # experimentalData is already processed (i.e. has node IDs rather than text node names)
            processedExperimentalData <- experimentalData
        } else {
            processedExperimentalData <- ProcessExperimentalData(experimentalData, network)
        }
        
        isCCG <- network$isCCG
        if (is.null(isCCG)) {
            # Network doesn't specify whether it's a CCG or not - assume it's a CG
            isCCG <- FALSE
        }
        
        ## By default, if in parallel mode use all cores bar one
        if (doParallel && is.null(numCores)) {
            numCores <- parallel::detectCores() - 1
            if (numCores < 1)
                numCores <- 1
        }
        
        if (isCCG) {
            numNodes <- igraph::gorder(network) / 2
        } else {
            numNodes <- igraph::gorder(network)
        }
        
        # If listOfNodes is null test all the hypotheses, otherwise only test the hypotheses in the list
        if (is.null(listOfNodes)) {
            numNodesToBeTested <- numNodes
            nodesToBeTested <- c(1:numNodesToBeTested)
        } else {
            numNodesToBeTested <- length(listOfNodes)
            # If listOfNodes is a character array convert to nodeIDs
            if (typeof(listOfNodes) == "character") {
                nodesToBeTested <- GetNodeID(network, listOfNodes)
            } else {
                nodesToBeTested <- listOfNodes
            }
        }
        
        if(!quiet) {
            cat(paste("Number of Nodes to analyse:", numNodesToBeTested, "\n"))
        }
        
        numPredictions <- nrow(processedExperimentalData)
        # Get the values of n+, n- and n0
        experimentalDataStats <- AnalyseExperimentalData(processedExperimentalData)
        
        if (!symmetricCCG) {
            # Add the downregulated nodes on to the list of hypotheses to test
            nodesToBeTested <- c(nodesToBeTested, nodesToBeTested + numNodes)
            numNodesToBeTested <- numNodesToBeTested * 2
        }

        ## Use lapply functions with GetScoresForSingleNode function to build scoresMatrix
        if (doParallel) {

            if (.Platform$OS.type == "windows") {
                ## Using parallel processing on windows. Windows does not have fork() so can not use mclapply. Use parLapply instead
                
                ## Create temp file for output  
                tempOutputFile <- paste0(tempdir(), "\\progressOutfile.txt")
                if(file.exists(tempOutputFile)) {
                    file.remove(tempOutputFile)
                }
				
                cat(paste("Parallel processing. Number of cores:", numCores, "\n"))
                if (!quiet && numNodesToBeTested > 100) {
                    ## Progress ouput only occurs every 100 nodes 
                    cat(paste("Writing progress statements to temporary file:", tempOutputFile, "\n"))
                }
                
                ## For n cores parLapply distributes the elements of nodeIndices to the cores using parallel::splitIndices (roughly, but not quite
                ## the first 1/n elements of nodeIndices to core 1, the second 1/n to core 2 etc.). Since the the low indices take longer to run than the
                ## higher indices arrange indices, here we put nodeIndices in order so they are they are evenly distributed, one by one with increasing nodeID
                ## across cores.
                ## Note that it is not currently known why the lower indices take longer than higher indices. It is suspected they are ordered
                ## by level of connectivity somehow/somewhere.
                nodeIndices <- parallel::splitIndices(numNodesToBeTested, numCores)
                x <- 1
                for (i in 1:max(do.call("c", lapply(nodeIndices, length)))) {
                    ## if there are less nodes to be tested than cores, only loop over that number
                    for (j in 1:min(numCores, numNodesToBeTested)) {
                        if (!is.na(nodeIndices[[j]][i])) {
                            nodeIndices[[j]][i] <- x
                            x <- x+1
                        }
                    }
                }
                nodeIndices <- do.call("c", nodeIndices)
                
                ## Manually set up the clusters, passing required packages with makeCluster and required functions and variables with clusterExport
                cl <- parallel::makeCluster(numCores, outfile = tempOutputFile)
#                parallel::clusterCall(cl, function() {requireNamespace("igraph")})
                parallel::clusterCall(cl, function() {requireNamespace("igraph"); requireNamespace("CausalR")})
                parallel::clusterExport(
                    cl, varlist = c(
                        ## CausalR functions - export these separately (rather than exporting the whole package so this can be run outside of the package)
#                         "GetScoresForSingleNode", "RemoveIDsNotInExperimentalData", "MakePredictions", "MakePredictionsFromCG", "MakePredictionsFromCCG",
#                         "AnalysePredictionsList","GetNumberOfPositiveAndNegativeEntries", "ScoreHypothesis", "CalculateSignificance",
#                         "CalculateSignificanceUsingCubicAlgorithm1b", "CalculateSignificanceUsingQuarticAlgorithm", "FindMaximumDValue", "FindApproximateValuesThatWillMaximiseDValue",
#                         "GetMaximumDValueFromTwoByTwoContingencyTable", "GetAllPossibleRoundingCombinations", "PopulateTheThreeByThreeContingencyTable",
#                         "CalculateWeightGivenValuesInThreeByThreeContingencyTable", "GetWeightsAboveHypothesisScoreForAThreeByTwoTable",
#                         "GetMaxDValueForAThreeByTwoFamily", "GetApproximateMaximumDValueFromThreeByTwoContingencyTable", "CalculateEnrichmentPValue",
#                         "GetSetOfSignificantPredictions", "GetSetOfDifferentiallyExpressedGenes", "FindIdsOfConnectedNodesInSubgraph",
#                         "GetMatrixOfCausalRelationships", "DetermineInteractionTypeOfPath", "GetCombinationsOfCorrectandIncorrectPredictions", "CheckPossibleValuesAreValid",
#                         "GetScoresWeightsMatrix", "GetScoreForNumbersOfCorrectandIncorrectPredictions", "GetWeightForNumbersOfCorrectandIncorrectPredictions",
#                         "ComputePValueFromDistributionTable",
                        ## Variables
                        "timeToRunSoFar", "nodesToBeTested", "network", "delta", "processedExperimentalData", "numPredictions", "epsilon",
                        "useCubicAlgorithm", "use1bAlgorithm", "symmetricCCG", "correctPredictionsThreshold", "experimentalDataStats", "quiet"
                    ), envir = environment()
                )
                
                ## get scoresMatrix by applying GetScoresForSingleNode to each node and joining with rbind
                scoresMatrix <- do.call(rbind, parallel::parLapply(cl, nodeIndices, function(x) {
                    GetScoresForSingleNode(
                        x, timeToRunSoFar, nodesToBeTested, network, delta, processedExperimentalData,
                        numPredictions, epsilon, useCubicAlgorithm, use1bAlgorithm, symmetricCCG,
                        correctPredictionsThreshold, experimentalDataStats, quiet
                    )
                }))

                ## Stop the clusters
                parallel::stopCluster(cl)
                
            } else {
                ## Using parallel processing on Linux/Mac. Uses fork() - use mclapply.
                nodeIndices <- 1:numNodesToBeTested
                
                ## get scoresMatrix by applying GetScoresForSingleNode to each node and joining with rbind
                scoresMatrix <- do.call(
                    rbind, parallel::mclapply(nodeIndices, function(x)
                        GetScoresForSingleNode(x, timeToRunSoFar, nodesToBeTested, network, delta, processedExperimentalData,
                                               numPredictions, epsilon, useCubicAlgorithm, use1bAlgorithm, symmetricCCG,
                                               correctPredictionsThreshold, experimentalDataStats, quiet
                        ), mc.cores = numCores)
                )
            }
        } else {
            ## Single threaded processing - use lapply.
            nodeIndices <- 1:numNodesToBeTested
            
            ## get scoresMatrix by applying GetScoresForSingleNode to each node and joining with rbind
            scoresMatrix <- do.call(rbind, lapply(nodeIndices, function(x)
                GetScoresForSingleNode(x, timeToRunSoFar, nodesToBeTested, network, delta, processedExperimentalData,
                                       numPredictions, epsilon, useCubicAlgorithm, use1bAlgorithm, symmetricCCG,
                                       correctPredictionsThreshold, experimentalDataStats, quiet
                )
            )
            )
        }
        
        ## First column of scores matrix contains the NodeID
        unsignedNodeNames <- GetNodeName(network, scoresMatrix[,1])
        
        nodeNames <- unsignedNodeNames
        nodeNames[scoresMatrix[,2] == 1] <- paste0(nodeNames[scoresMatrix[,2] == 1], "+")
        nodeNames[scoresMatrix[,2] == -1] <- paste0(nodeNames[scoresMatrix[,2] == -1], "-")
            
        # Sort the output table in order of score (decreasing), then regulation (decreasing), then nodeID (increasing)
        orderHypotheses <- order(-scoresMatrix[, 3], -scoresMatrix[, 2], scoresMatrix[, 1])
        rankedHypotheses <- data.frame(scoresMatrix, row.names = nodeNames)
        colnames(rankedHypotheses) <- c("NodeID", "Regulation", "Score", "Correct", "Incorrect", "Ambiguous", "p-value", "Enrichment p-value")
        
        rankedHypotheses <- rankedHypotheses[orderHypotheses,]
        
        # Save results to an text file
        if(writeFile) {
            rhoutput <- scoresMatrix[, c(1, 1:8)]
            rhoutput[, 1] <- unsignedNodeNames
            rhoutput <- rhoutput[orderHypotheses,]
            colnames(rhoutput) <- c("NodeName", "NodeID", "Regulation", "Score", "Correct", "Incorrect", "Ambiguous", "p-value", "Enrichment p-value")
            utils::write.table(rhoutput, paste0(outputDir, "/ResultsTable-", fileOutputName, ".txt"), sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
        return(rankedHypotheses)
    }
