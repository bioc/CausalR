#' @title Write explained nodes to Sif file
#' @description
#' Outputs a network of explained nodes in .sif file format for producing visualisations using Cytoscape
#' @export
#' @param hypothesisnode a hypothesis node
#' @param signOfHypothesis the direction of change of hypothesis node
#' @param network a computational causal graph
#' @param data a data file
#' @param delta the number of edges across which the hypothesis should be followed
#' @param file a character string (without extension) that determines the names of the files created. Extension is added automatically. Set to NA if not writing to file.
#' @param display determines if the output written to file is also displayed
#' @return two files containing paths from hypothesis node to explained nodes in sif format
#' @export
#' @concept CausalR
#' @examples
#' hypothesisnode <- "Node0"
#' signOfHypothesis <- +1
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
#' ccg <- CreateCCG(network)
#' data <- system.file(package='CausalR', 'extdata', 'testData1.txt')
#' delta <- 2
#' WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, ccg, data,  delta,
#'                              file=NA, display=TRUE)

WriteExplainedNodesToSifFile <- function(hypothesisnode, signOfHypothesis, network, data, delta, file, display = TRUE) {
    
    ## Initialise empty lists. Each will have one element for each delta
    ## In explainedNodes each element will be a table of explained nodes (I.e. nodes that appear in both network and data with the same sign).
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
        explainedNodes[[thisDelta]] <- GetExplainedNodesOfCCG(hypothesisnode, +1, network, data,  thisDelta)
        
        ## Get explained nodes from this delta only (not lower delta). I.e. remove nodes that are listed in previous delta
        if (thisDelta == 1) {
            uniqueExplainedNodes[[thisDelta]] <- explainedNodes[[thisDelta]]
        } else {
            uniqueExplainedNodes[[thisDelta]] <- explainedNodes[[thisDelta]][!(explainedNodes[[thisDelta]][,1] %in% explainedNodes[[thisDelta-1]][,1]),]
        }
        
        ## For each explained node for thisDelta, excluding the hypothesis node itself, get the shortest paths to it from the hypothesisnode
        if(nrow(uniqueExplainedNodes[[thisDelta]]) > 0) {
            pathsToUniqueExplainedNodes[[thisDelta]] <- do.call("rbind", unlist(
                lapply(uniqueExplainedNodes[[thisDelta]][uniqueExplainedNodes[[thisDelta]] != hypothesisnode],
                       function(x) GetShortestPathsFromCCG(network, hypothesisnode, x, display = FALSE)), recursive = FALSE))
        }
    }
    
    ## Convert the paths into sif format 
    output <- do.call("rbind", lapply(pathsToUniqueExplainedNodes, GetPathsInSifFormat))
    output <- sort(unique(output))
         
    ## Combine list elements
    output2 <- do.call("rbind", uniqueExplainedNodes)
    output2 <- unique(output2)
    output2 <- output2[ order(-output2[,2], output2[,1]), ]
    colnames(output2) <- c("NodeID", "Regulation")
    
    if (!is.na(file)) {
        cat("Writing sif file to: ", paste0(file, ".sif"), "\n" , sep="")
        cat("Writing annotation file to: ", paste0(file, "_anno.txt"), "\n", sep="")
        write(output, paste0(file, ".sif"), sep = "\n")
        utils::write.table(output2, paste0(file, "_anno.txt"), row.names = FALSE, quote = FALSE, sep = "\t")
    }
    
    if (display) {
        cat(output, sep = "\n")
    }
    

}
