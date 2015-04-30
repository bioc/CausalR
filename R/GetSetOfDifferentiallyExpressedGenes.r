#' @title get set of differientially expressed genes
#' @description
#' Gets the set of differentially expressed genes in the results, G+ as defined by in Causal reasoning on biological networks: Interpreting transcriptional changes,  L Chindelevitch et al.
#' @param results a table of results
#' @return a matrix of differentially expressed genes

#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.

GetSetOfDifferentiallyExpressedGenes <- function(results){

counter <- 1
numResults <- nrow(results)
differentiallyExpressedGenes <- matrix(0,numResults,1)
for (i in 1:numResults){
	if ((as.numeric(results[i,2]) != 0)){
		differentiallyExpressedGenes[counter,1] <- results[i, 1]
		counter <- counter + 1
	}
}
differentiallyExpressedGenes <- differentiallyExpressedGenes[1:counter-1]

return(differentiallyExpressedGenes)
}