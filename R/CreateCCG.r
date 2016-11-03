#' @title create a Computational Causal Graph (CCG)
#' @description
#' Creates a computational causal graph from a network file.
#' @export
#' @concept CausalR
#' @param filename file name of the network file (in .sif file format)
#' @param nodeInclusionFile optional path to a text file listing nodes to exclude in the CCG (or include - see argument excludeNodesInFile). 
#' @param excludeNodesInFile flag to determine if nodes in inclusion file should be taken as nodes to include or nodes to exclude. Default is TRUE to exclude.
#' @return an igraph object containing the CCG.
#' @examples
#' # get path to example .sif file
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' #create ccg
#' ccg = CreateCCG(network)

#' 
#' 
#' @usage CreateCCG(filename, nodeInclusionFile = NULL, excludeNodesInFile = TRUE)
#' 
#' @note CreateCG and CreateCCG create causal and computational causal graphs respectively.
#' 
#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.



CreateCCG <- function(filename, nodeInclusionFile = NULL, excludeNodesInFile = TRUE) {
    # Create a Computational Causal Graph containing separate +ve and -ve nodes from a SIF file
    
    # Note that this is a CCG in the sense that the Chindelevitch et al paper uses.  Generally in this code we use a CCG to mean the matrix of
    # interactions.
    
    
    tableOfInteractions <- ReadSifFileToTable(filename)

    if (!is.null(nodeInclusionFile)) {
        # If a node 'inclusion file' has been specified get a vector of TRUE/FALSE, where TRUE corresponds
        # to rows that contain at least one of the nodes listed in the file
        inclusionNodes <- scan(nodeInclusionFile, what = "", quiet = TRUE)
        inclusionNodesFound <- tableOfInteractions[,1] %in% inclusionNodes | tableOfInteractions[,3] %in% inclusionNodes 
        
        if(excludeNodesInFile) {
            tableOfInteractions <- tableOfInteractions[!inclusionNodesFound,]
        } else {
            tableOfInteractions <- tableOfInteractions[inclusionNodesFound,]
        }
    }
    
    if(nrow(tableOfInteractions) < 1) {
        stop("No interactions found in network file (or all interactions are excluded by nodeInclusionFile)")
    }
    
    nEdges <- dim(tableOfInteractions)[1]
    
    nodeNames <- unique(c(tableOfInteractions[, 1], tableOfInteractions[, 3]))
    
    # Alternative approach to getting the node names which puts them in the same order that creating an igraph directly from tableOfInteractions would:
    # temp <- matrix(c(tableOfInteractions[,1],tableOfInteractions[,3])) dim(temp) <- c(nEdges,2) temp <- t(temp) dim(temp) <- c(1,nEdges*2) nodeNames
    # <- unique(matrix(temp))
    
    nNodes <- length(nodeNames)
    
    # Create a list of signed node names, i.e. 'node0+', 'node1+', ..., 'node0-', 'node1-', ...
    nodeNamesSigned <- c(nodeNames, nodeNames)
    for (i in 1:nNodes) {
        nodeNamesSigned[i] <- paste(nodeNamesSigned[i], "+", sep = "")
    }
    for (i in (nNodes + 1):(nNodes * 2)) {
        nodeNamesSigned[i] <- paste(nodeNamesSigned[i], "-", sep = "")
    }
    
    # Create a matrix of the edges of the CCG Two entries per line in the SIF file - one from nodeX+ and the other from nodeX-
    edgeList <- matrix(0, nEdges * 2, 2)
    for (i in 1:nEdges) {
        startNode <- which(tableOfInteractions[i, 1] == nodeNames)
        endNode <- which(tableOfInteractions[i, 3] == nodeNames)
        edgeList[i, 1] <- startNode
        edgeList[i + nEdges, 1] <- startNode + nNodes
        if (tableOfInteractions[i, 2] == "Activation" || tableOfInteractions[i, 2] == "Activates") {
            edgeList[i, 2] <- endNode
            edgeList[i + nEdges, 2] <- endNode + nNodes
        } else if (tableOfInteractions[i, 2] == "Inhibition" || tableOfInteractions[i, 2] == "Inhibits") {
            edgeList[i, 2] <- endNode + nNodes
            edgeList[i + nEdges, 2] <- endNode
        }
    }
    
    # Now create the igraph, and add the node names as a property to each vertex
    network <- igraph::graph_from_edgelist(edgeList, TRUE)
    igraph::V(network)$name <- nodeNamesSigned
    
    # give the network a name attribute, set to the filename minus the path and file extension
    network$name <- tools::file_path_sans_ext(basename(filename))
    
    network$isCCG <- TRUE
    igraph::V(network)$unsignedName <- c(nodeNames, nodeNames)
    
    return(network)
} 
