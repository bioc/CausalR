#' @title get weights from interaction information
#' @description
#' Returns a matrix of weights (-1,0,+1) indicating the direction of regulation from the interaction information.
#' @param interactionInfo a central column of the .sif file, giving the type of edge interaction
#' @return a matrix of weights corresponding the the direction of regulation

GetWeightsFromInteractionInformation <- function(interactionInfo) {
    
    
    numEdges <- length(interactionInfo)
    matrixOfWeight <- matrix(0, numEdges, 1)
    # If the interaction is activation, the weight assigned is 1, if the interaction is inhibition the weight assigned is -1.
    matrixOfWeight[interactionInfo == "Activates"] <- 1
    matrixOfWeight[interactionInfo == "Activation"] <- 1
    matrixOfWeight[interactionInfo == "Inhibits"] <- -1
    matrixOfWeight[interactionInfo == "Inhibition"] <- -1
    
    return(matrixOfWeight)
} 
