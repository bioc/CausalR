#' @title create a Computational Causal Graph (CCG)
#' @description
#' Creates a computational causal graph from a network file.
#' @export
#' @param filename file name of the network file (in .sif file format)
#' @return an igraph object containing the CCG.
#' @examples
#' # get path to example .sif file
#' network <- system.file(package="CausalR", "extdata", "testNetwork.sif")
#' #create ccg
#' ccg = CreateCCG(network)

#' 
#' 
#' @usage CreateCCG(filename)
#' 
#' @note CreateCG and CreateCCG create causal and computational causal graphs respectively.
#' 
#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.



CreateCCG <- function(filename){
  # Create a Computational Causal Graph containing separate +ve and -ve nodes from a SIF file
  
  # Note that this is a CCG in the sense that the Chindelevitch et al paper uses.
  # Generally in this code we use a CCG to mean the matrix of interactions.
  
  
  tableOfInteractions <- ReadSifFileToTable(filename)
  
  nEdges <- dim(tableOfInteractions)[1] 
  
  nodeNames <- unique(c(tableOfInteractions[,1],tableOfInteractions[,3]))
  
  # Alternative approach to getting the node names which puts them in the same order that creating an igraph directly from tableOfInteractions would:
  #temp <- matrix(c(tableOfInteractions[,1],tableOfInteractions[,3]))
  #dim(temp) <- c(nEdges,2)
  #temp <- t(temp)
  #dim(temp) <- c(1,nEdges*2)
  #nodeNames <- unique(matrix(temp))
  
  nNodes <- length(nodeNames)
  
  # Create a list of signed node names, i.e. "node0+", "node1+", ..., "node0-", "node1-", ...
  nodeNamesSigned <- c(nodeNames,nodeNames)
  for (i in 1:nNodes){
    nodeNamesSigned[i] <- paste(nodeNamesSigned[i],"+",sep="")
  }
  for (i in (nNodes+1):(nNodes*2)){
    nodeNamesSigned[i] <- paste(nodeNamesSigned[i],"-",sep="")
  }
  
  # Create a matrix of the edges of the CCG
  # Two entries per line in the SIF file - one from nodeX+ and the other from nodeX-
  edgeList <- matrix(0,nEdges*2,2)
  for (i in 1:nEdges){
    startNode <- which(tableOfInteractions[i,1] == nodeNames)
    endNode <- which(tableOfInteractions[i,3] == nodeNames)
    edgeList[i,1] <- startNode
    edgeList[i+nEdges,1] <- startNode + nNodes
    if (tableOfInteractions[i,2] == "Activation" || tableOfInteractions[i,2] == "Activates"){
      edgeList[i,2] <- endNode
      edgeList[i+nEdges,2] <- endNode + nNodes
    }
    else if (tableOfInteractions[i,2] == "Inhibition" || tableOfInteractions[i,2] == "Inhibits"){
      edgeList[i,2] <- endNode + nNodes
      edgeList[i+nEdges,2] <- endNode
    }
  }
  
  # Now create the igraph, and add the node names as a property to each vertex
  network <- graph.edgelist(edgeList, TRUE)
  V(network)$name <- nodeNamesSigned
  
  network$isCCG <- TRUE
  V(network)$unsignedName <- c(nodeNames,nodeNames)
  
  return(network)
}
