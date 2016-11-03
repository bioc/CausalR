#' @title create a Computational Graph (CG)
#' @description
#' Creates a CG network from a .sif file.
#' Takes in a .sif file output from Cytoscape, and creates an 'igraph' representing the network.
#' The edges will be annotated with the type of interaction and a weight (1 for activation and -1 for inhibition)


#' @param sifFile the path of the .sif file that contains all 
#'       the information about the network Load in .sif file
#' @return a CG network
#' @export
#' @concept CausalR
#' @examples
#' # get path to example .sif file
#' network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#' #create cg
#' cg = CreateCG(network)

CreateCG <- function(sifFile) {
    
    tableOfInteractions <- ReadSifFileToTable(sifFile)
    print(paste("File read complete - read in", nrow(tableOfInteractions), "lines. Now constructing network", sep = " "))
    
    # Convert input data into a network
    network <- CreateNetworkFromTable(tableOfInteractions)
    print("Network has been created - now adding edge properties")
    
    # Add edge information
    interactionInformation <- GetInteractionInformation(tableOfInteractions)
    network <- AddWeightsToEdges(network, interactionInformation)
    print("Added weights to edges")
    
    network <- AddIDsToVertices(network)
    
    # give the network a name attribute, set to the filename minus the path and file extension
    network$name <- tools::file_path_sans_ext(basename(sifFile))
    
    network$isCCG <- FALSE
    
    return(network)
} 
