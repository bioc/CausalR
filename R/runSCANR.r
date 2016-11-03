#' @title  run ScanR
#' @description 
#' This function will return nodes regulated by the given hypothesisGene
#' @param network Computational Causal Graph, as an igraph.
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. The results is an n x 2 matrix; where the first column contains the node ids of the nodes in the network that the results refer to. The second column contains values indicating the direction of regulation in the results - (+)1 for up, -1 for down and 0 for insignificant amounts of regulation.
#' @param numberOfDeltaToScan Iteratively scan for 1 to numberOfDeltaToScan delta values
#' @param topNumGenes A value to select top genes to report (typically top 100 genes)
#' @param correctPredictionsThreshold Minimal score for p-values calculation. Value is passed to RankTheHypothesis - scores below this value will get NAs for p-value and enrichment p-value. The default is Inf, so that no p-values are calculated.
#' @param writeResultFiles If set to TRUE the results of the scan will be written to two text files in the working directory. Default is TRUE.
#' @param writeNetworkFiles If set to "all" .sif files and corresponding _anno.txt files will be generated for the top correctly explained, incorrectly explained and ambiguously explained nodes. If set to "correct" they will only be calculated for correctly explained nodes. If set to "none", no networks will be generated. Default is "all".
#' @param doParallel A flag for running RankTheHypothesis in parallel mode. Default is FALSE.
#' @param numCores Number of cores to use if using parallel mode. If the default value of NULL is used, it will attempt to detect the number of cores available and use all of them bar one.
#' @param quiet a flag to suppress output to console. FALSE by default.
#' @param outputDir the directory to output the files to. Default is the working directory
#' @return returns list of genes from each delta scan run
#' @export
#' @concept CausalR
#' @examples
#' numberOfDeltaToScan <- 2
#' topNumGenes <- 4
#' #get path to example network file
#' networkFile <- system.file(package = 'CausalR', 'extdata', 'testNetwork.sif')
#' #create ccg
#' network <-  CreateCCG(networkFile)
#' #get path to example experimental data
#' experimentalDataFile <- system.file(package = 'CausalR', 'extdata', 'testData.txt')
#' #read in experimetal data
#' experimentalData <-  ReadExperimentalData(experimentalDataFile, network)
#' #run in single threaded mode
#' runSCANR(network, experimentalData, numberOfDeltaToScan, topNumGenes)
#' #run in parallel mode
#' runSCANR(network, experimentalData, numberOfDeltaToScan, topNumGenes,
#'          doParallel = TRUE, numCores = 2)


runSCANR <- function(network, experimentalData, numberOfDeltaToScan = 5, topNumGenes = 150, correctPredictionsThreshold = Inf, writeResultFiles = TRUE, writeNetworkFiles = "all", doParallel=FALSE, numCores=NULL, quiet = FALSE, outputDir = getwd()) {
    
    numberOfDeltaToScan <- as.integer(numberOfDeltaToScan)
    topNumGenes <- as.integer(topNumGenes)
    
    # If outputDir doesn't exist, create it
    if (!file.exists(file.path(outputDir))){
        dir.create(file.path(outputDir))
    }
    
    if (writeNetworkFiles == "all") {
        writeNetworkFiles <- TRUE
        correctlyExplainedOnly <- FALSE
    } else if (writeNetworkFiles == "correct") {
        writeNetworkFiles <- TRUE
        correctlyExplainedOnly <- TRUE
    } else if (writeNetworkFiles == "none"){
        writeNetworkFiles <- FALSE
    } else {
        stop("writeNetworkFiles should be set to either \"all\", \"correct\" or \"none\"")
    }
    
    resultsFileOutputName <- paste0(network$name, "-", colnames(experimentalData)[1], "-deltaScanned", numberOfDeltaToScan, "-top", topNumGenes)
    
    if (as.integer(numberOfDeltaToScan) > 0) {
        deltaToScan <- 1:numberOfDeltaToScan
    } else {
        stop("numberOfDeltaToScan must be an integer > 0")
    }
    
    # Create empty data frame and set column names to delta scan values
    testList <- data.frame(matrix(nrow = topNumGenes, ncol = numberOfDeltaToScan))
    colnames(testList) <- deltaToScan
    
    # Run RankTheHypotheses on each delta value and fill testList
    for (delta in deltaToScan) {
        rankedHypo <- RankTheHypotheses(network, experimentalData, delta, correctPredictionsThreshold, doParallel=doParallel, numCores=numCores, writeFile = FALSE, quiet = TRUE)
        testList[,delta] <- row.names(rankedHypo)[1:topNumGenes]
    }
    
    commonNodes <- Reduce(intersect, testList) # Nodes that appear in top 'topNumGenes' in every delta value
    uniqueNodes <- data.frame(names = unique(unlist(testList)), maxDelta = NA) # Nodes that appear in at least one list
    
    for (i in 1:nrow(uniqueNodes)) {
        # Find which columns contain this gene
        colsIn <- apply(testList, 2, function(r) any(r %in% uniqueNodes$names[i]))
        # Get the names of these columns as integers, and find the max
        uniqueNodes$maxDelta[i] <- max(as.integer(colnames(testList))[colsIn])
    }
    
    if (writeResultFiles) {
        if(!quiet) {
            cat("Writing TopNodes file to: ", paste0(outputDir, "/TopNodes-", resultsFileOutputName, ".txt"), "\n" , sep="")
            cat("Writing CommonNodes file to: ", paste0(outputDir, "/CommonNodes-", resultsFileOutputName, ".txt"), "\n", sep="")
        }
        utils::write.table(uniqueNodes, file = paste0(outputDir, "/TopNodes-", resultsFileOutputName, ".txt"), sep = "\t", row.names = FALSE, quote = FALSE)
        write(commonNodes, file = paste0(outputDir, "/CommonNodes-", resultsFileOutputName, ".txt"))
    }
    
    
    if (writeNetworkFiles) {
        for (node in commonNodes) {
            # get separate node name and regulation sign from the combined string
            negativeSign <- unlist(gregexpr(pattern ='-', text = node, fixed = TRUE))
            positiveSign <- unlist(gregexpr(pattern ='+', text = node, fixed = TRUE))
            
            if (negativeSign == nchar(node) & positiveSign < 0) {
                hypothesisnode <- substr(node, 1, nchar(node)-1)
                signOfHypothesis <- -1
            } else if (positiveSign == nchar(node) & negativeSign < 0) {
                hypothesisnode <- substr(node, 1, nchar(node)-1)
                signOfHypothesis <- +1
            } else {
                stop("Cannot deduce sign of hypothesis from node")
            }
            
            networkFilesOutputName <- paste0(network$name, "-", colnames(experimentalData)[1], "-delta", delta, "-top", topNumGenes, "-", node)
            #trimws(paste(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "My files"))
            
            WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData, delta = numberOfDeltaToScan, outputFilesName = networkFilesOutputName, correctlyExplainedOnly = correctlyExplainedOnly, quiet = quiet, outputDir = outputDir)
        }
    }

    return(list(testList = testList, uniqueNodes = uniqueNodes, commonNodes = commonNodes))
} 
