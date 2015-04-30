#' @title get shortest paths from CCG
#' @description
#' Gets the node names in the shortest path from one node in a CCG to another
#' @param network built from iGraph
#' @param hypothesisnode hypothesis node ID
#' @param targetnode target node ID
#' @param showbothdirs where multiple paths from a positive and negative node, FALSE returns only the shortest. Otherwise both are returned.
#' @param display if true, echo the rsulting paths to the screen
#' @return a list of vectors containing the nodes of individual paths
#' @export
#' @examples
#' network <- system.file(package="CausalR", "extdata", "testNetwork.sif")
#' ccg = CreateCCG(network)
#' hypothesisnode = 1
#' targetnode = 10
#' GetShortestPathsFromCCG (ccg, hypothesisnode, targetnode)


GetShortestPathsFromCCG <- function(network, hypothesisnode, targetnode, showbothdirs=FALSE, display=TRUE){
  
  if (is.numeric(hypothesisnode)){
    hypothesis <- hypothesisnode
  }
  else{
    hypothesis <- GetNodeID(network, hypothesisnode)
  }
  
  if (is.numeric(targetnode)){
    targetID <- targetnode
  }
  else{
    targetID <- which(V(network)$unsignedName == targetnode)
    if (!any(targetID)){
      targetID <- which(V(network)$name == targetnode)
    }
  }
  
  shortestPath <- shortest.paths(network, hypothesis, targetID, mode="out")
  
  # If there are no paths from hypothesis to target (+ or -), return NULL
  if (all(shortestPath == Inf)){
    return(NULL)
  }
  
  # If there is more than one target (i.e. a + and a - node), decide which one(s) to include:
  # - don't include one which has no path from the hypothesis node
  # - include the one which is closer to the hypothesis node (both if the two paths are equal)
  # - include the one which is further from the hypothesis node if showbothdirs is true (i.e. flag to override the default "show only the closer node" behaviour)
  if (length(shortestPath) > 1){
    if (shortestPath[1] == Inf){
      targetID <- targetID[2]
    }
    else{
      if (shortestPath[2] == Inf){
        targetID <- targetID[1]
      }
      else{
        if (!showbothdirs){
          if (shortestPath[1] < shortestPath[2]){
            targetID <- targetID[1]
          }
          else{
            if (shortestPath[2] < shortestPath[1]){
              targetID <- targetID[2]
            }
          }
        }
      }
    }
  }
  
  # Now assemble a list of the nodes in the paths from the hypothesis to the targets selected above
  nodesinpath <- list()
  inode <- 0
  for (itarget in 1:length(targetID)){
    shortestPaths <- get.all.shortest.paths(network, hypothesis, targetID[itarget], mode="out")
    for (i in 1:length(shortestPaths$res)){
      inode <- inode + 1
      nodesinpath[[inode]] <- V(network)$name[shortestPaths$res[[i]]]
    }    
  }
  
  if (display){
    for (i in 1:length(nodesinpath)){
      for (j in 1:length(nodesinpath[[i]])){
        cat(nodesinpath[[i]][j], '\t', sep='')
      }
      cat('\b\n')
    }
    return(invisible(nodesinpath))
  }
  else{
    return(nodesinpath)
  }  
}