#' @title order hypotheses
#' @description
#' Ranks the hypotheses.  Takes a matrix containing the scores for each node of the network, and ranks them placing the hypothesis with the 
#' most correct predictions is at the top

#' @param scoresMatrix a matrix containing the scores for each node of the network
#' @return a ranked table of hypotheses

OrderHypotheses <- function(scoresMatrix) {
    
    tableOfScores <- as.table(scoresMatrix)
    # Order in descending order
    orderedTable <- tableOfScores[order(-1 * (as.numeric(tableOfScores[, 3]))), ]
    
    return(orderedTable)
} 
