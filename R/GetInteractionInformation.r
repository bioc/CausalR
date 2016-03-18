#' @title returns interaction information from input data
#' @description
#' Gets the interaction information from the input data
#' @param dataTable a data table containing the information read in from the .sif file representing the network.
#' @return a vector of interaction information

GetInteractionInformation <- function(dataTable) {
    
    informationMatrix <- as.matrix(dataTable)
    interactionInfo <- informationMatrix[, 2]
    return(interactionInfo)
} 
