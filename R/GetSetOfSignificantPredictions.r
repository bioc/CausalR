#' @title get set of significant predictions
#' @description 
#' Gets the set of positive and negative predictions, the combination of the sets Sh+ and Sh- in Causal reasoning on biological networks: Interpreting transcriptional changes,  L Chindelevitch et al.
#' @param predictions a table of predictions
#' @return a matrix of positive and negative predictions

#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.

GetSetOfSignificantPredictions <- function(predictions) {
    
    counter <- 1
    numPredictions <- nrow(predictions)
    significantPredictions <- matrix(0, numPredictions, 1)
    for (i in 1:numPredictions) {
        if ((as.numeric(predictions[i, 2]) != 0)) {
            significantPredictions[counter, 1] <- predictions[i, 1]
            counter <- counter + 1
        }
    }
    significantPredictions <- significantPredictions[1:counter - 1]
    
    return(significantPredictions)
} 
