#' @title Get paths in Sif format
#' @description
#' Converts network paths into Simple interaction file (.sif) format for importing into Cytoscape

#' @param arrayOfPaths an array of paths (in the format outputted by GetShortestPathsFromCCG) to be converted to .sif format
#' @return network visualisation

GetPathsInSifFormat <- function(arrayOfPaths)  {
    
    listofstuff <- NULL
    
    for (i in 1:nrow(arrayOfPaths)) {
        for (j in 2:ncol(arrayOfPaths)) {
            
            from <- arrayOfPaths[i,j-1]
            to <- arrayOfPaths[i,j]
            
            finalChars <- paste(substr(from, nchar(from), nchar(from)), substr(to, nchar(to), nchar(to)), sep="")
            
            if (finalChars=="++" || finalChars=="--") {
                word = "Activates"
            } else if (finalChars=="+-" || finalChars=="-+"){
                word = "Inhibits"
            } else {
                stop(paste("Error: Nodes should end in either + or -"))
            }
            
            listofstuff <- rbind(listofstuff, paste(substr(arrayOfPaths[i,j-1], 1, nchar(arrayOfPaths[i,j-1])-1), word, substr(arrayOfPaths[i,j], 1, nchar(arrayOfPaths[i,j])-1), sep="\t"))
        }
    }
    return(unique(listofstuff))
}
