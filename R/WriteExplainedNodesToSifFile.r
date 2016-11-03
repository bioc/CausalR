#' @title Write explained nodes to Sif file
#' @description
#' Outputs networks of explained nodes in .sif file format for producing visualisations using Cytoscape. Output will be to a
#' directory beginning with a timestamp, 

#' @param hypothesisnode a hypothesis node
#' @param signOfHypothesis the direction of change of hypothesis node
#' @param network a computational causal graph
#' @param experimentalData The experimental data read in using \link{ReadExperimentalData}. The results is an n x 2 matrix; where the first column contains the node ids of the nodes in the network that the results refer to. The second column contains values indicating the direction of regulation in the results - (+)1 for up, -1 for down and 0 for insignificant amounts of regulation. The name of the first column is the filename the data was read from.
#' @param delta the number of edges across which the hypothesis should be followed
#' @param outputDir the directory to output the files to. Default is the working directory
#' @param outputFilesName a character string to use for the name of the output files. Default value is "", which results in files using the default naming convention of "network file name-data file name-delta value-node name". Set to NA if not writing to file.
#' @param correctlyExplainedOnly if TRUE network files will only be produced for correctly explained nodes. If FALSE network files will be produced for each of correctly explained, incorrectly explained and ambiguously explained nodes.
#' @param quiet a flag to suppress output to console. FALSE by default.
#' @return files containing paths from hypothesis node to explained nodes in sif format and corresponding annotation (_anno.txt) files
#' @export
#' @concept CausalR
#' @examples
#' hypothesisnode <- "Node0"
#' signOfHypothesis <- +1
#' networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
#' network <- CreateCCG(networkFile)
#' experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
#' experimentalData <- ReadExperimentalData(experimentalDataFile, network)
#' delta <- 2
#' WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData, delta,
#'                              outputFilesName=NA)


WriteExplainedNodesToSifFile <- function(hypothesisnode, signOfHypothesis, network, experimentalData, delta, outputDir = getwd(), outputFilesName = "", correctlyExplainedOnly = FALSE, quiet = FALSE) {
    
    signOfHypothesis <- as.integer(signOfHypothesis)
    delta <- as.integer(delta)
    
    if(correctlyExplainedOnly) {
        nodeSetsToProcess <- c("corExplainedNodes")
    } else {
        nodeSetsToProcess <- c("corExplainedNodes", "incorExplainedNodes", "ambExplainedNodes")
    }
    
    # If outputDir doesn't exist, create it
    if (!file.exists(file.path(outputDir))){
        dir.create(file.path(outputDir))
    }
    
    for (thisNodeSet in nodeSetsToProcess) {
        
        ## Initialise empty lists. Each will have one element for each delta
        ## In explainedNodes each element will be a table of explained nodes (I.e. nodes that appear in both network and experimentalData with the same sign).
        ## The table will contain the node name and the value (1 or -1)
        ## In uniqueExplainedNodes each element will be a table that is a subset of that in explainedNodes, where entries that appear in the previous
        ## (lower delta) tables have been removed
        ## In pathsToUniqueExplainedNodes each element will be a table that contains the same nodes as described in uniqueExplainedNodes but each row will contain
        ## the path to that node from hypothesis node.
        explainedNodes <- list()
        uniqueExplainedNodes <- list()
        pathsToUniqueExplainedNodes <- list()
        
        for (thisDelta in 1:delta) {
            
            ## Get a table of explained nodes and their values within thisDelta 
            explainedNodes[[thisDelta]] <- GetExplainedNodesOfCCG(hypothesisnode, signOfHypothesis, network, experimentalData, thisDelta)[[thisNodeSet]]
            
            ## Get explained nodes from this delta only (not lower delta). I.e. remove nodes that are listed in previous delta
            if (thisDelta == 1) {
                uniqueExplainedNodes[[thisDelta]] <- explainedNodes[[thisDelta]]
            } else {
                uniqueExplainedNodes[[thisDelta]] <- explainedNodes[[thisDelta]][!(explainedNodes[[thisDelta]][,1] %in% explainedNodes[[thisDelta-1]][,1]),]
            }
            
            ## For each unique explained node for thisDelta, excluding the hypothesis node itself, get the shortest paths to it from the hypothesisnode
            if(nrow(uniqueExplainedNodes[[thisDelta]]) > 0) {
                targetNodes <- uniqueExplainedNodes[[thisDelta]]$name[uniqueExplainedNodes[[thisDelta]]$name != hypothesisnode]
                if (length(targetNodes) > 0) {
                pathsToUniqueExplainedNodes[[thisDelta]] <- do.call("rbind",
                    unlist(lapply(targetNodes, function(targetNode) GetShortestPathsFromCCG(network, hypothesisnode, targetNode, quiet = TRUE)), recursive = FALSE)
                )
                } else {
                    pathsToUniqueExplainedNodes[[thisDelta]] <- NULL
                }
            }
        }
        
        ## Convert the paths into sif format 
        if (length(pathsToUniqueExplainedNodes) > 0) {
            output <- do.call("rbind", lapply(pathsToUniqueExplainedNodes, GetPathsInSifFormat))
            output <- sort(unique(output))
        } else {
            output <- NULL
        }
        
        ## Combine list elements
        output2 <- do.call("rbind", uniqueExplainedNodes)
        output2 <- unique(output2)
        output2 <- output2[ order(-output2[,2], output2[,1]), c("name", "exp")]
        colnames(output2) <- c("NodeID", "Regulation")
        
        if (signOfHypothesis < 0) {
            node <- paste0(hypothesisnode, "-")
        } else {
            node <- paste0(hypothesisnode, "+")
        }
        
        if (!is.na(outputFilesName)) {
            if(outputFilesName == "") {
                outputFilesName <- paste0(network$name, "-", colnames(experimentalData)[1], "-delta", delta, "-", node)
            }
            if(!quiet) {
                cat("Writing sif file to: ", paste0(outputDir, "/", thisNodeSet, "-", outputFilesName, ".sif"), "\n" , sep="")
                cat("Writing annotation file to: ", paste0(outputDir, "/", thisNodeSet, "-", outputFilesName, "_anno.txt"), "\n", sep="")
            }
            write(output, paste0(outputDir, "/", thisNodeSet, "-", outputFilesName, ".sif"), sep = "\n")
            utils::write.table(output2, paste0(outputDir, "/", thisNodeSet, "-", outputFilesName, "_anno.txt"), row.names = FALSE, quote = FALSE, sep = "\t")
        }
        
        if (!quiet) {
            cat(thisNodeSet , "\n")
            cat(output, sep = "\n")
        }
    }
}
