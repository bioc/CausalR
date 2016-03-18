#' @title read experimental data
#' @description 
#' Reads experimental data for the causal reasoning algorithm from a text file.
#' @usage ReadExperimentalData(fileName, network, removeDuplicates)



#' @export
#' @concept CausalR
#' @param fileName a file containing the experimental data (text file format)
#' @param network  a (Computational) Causal Graph, as an igraph.
#' @param removeDuplicates Optional, defaults to true. Remove duplicated nodes the experimental file (i.e. where the result for a node is repeated, use the first value given only; the alternative is to return a result which contains multiple rows for this node).
#' @return (n x 2) matrix of nodes and direction of regulation. The first column of the matrix contains the node IDs from the network, and the second contains the experimental values.
#' @examples
#' #get path to example network file
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' ##create ccg
#' ccg <- CreateCCG(network)
#' #get path to example experimental data
#' fileName<- system.file(package='CausalR', 'extdata', 'testData.txt')

#' ReadExperimentalData(fileName, ccg)





ReadExperimentalData <- function(fileName, network, removeDuplicates = TRUE) {
    
    if (!(igraph::is_igraph(network) || is.null(network))) {
        stop("network input must be an igraph")
    }
    
    table <- utils::read.table(fileName, colClasses = "character")
    
    isValid <- ValidateFormatOfDataTable(table)
    
    if (!isValid) {
        stop()
    }
    
    # Check for / deal with genes which are listed more than once
    if (anyDuplicated(table[, 1])) {
        if (removeDuplicates) {
            warning("Some genes have more than one result in ", fileName, " - duplicates have been removed by keeping the first result in the file.")
            table <- table[!duplicated(table[, 1]), ]
        } else {
            warning("Some genes have more than one result in ", fileName, " - duplicates have not been removed, scoring functions will not work.")
        }
    }
    
    if (is.null(network)) {
        # No network info provided - return the raw table (Note that this option is provided purely for testing purposes, e.g. to make sure that the table
        # is being read in correctly, and is not intended to be used by an end user.)
        return(table)
    } else {
        processedData <- ProcessExperimentalData(table, network)
        return(processedData)
    }
} 
