#' @title counts the number of positive and negative entries
#' @description
#' Counts the number of entries in the in the second column of an input table that are +1 or -1.
#' 
#' @param dataList an array or dataframe in which the second column is numeric
#' @return a vector of two components, the first of which giving the number of +1 entries, the second the number of -1's.
#' @export
#' @concept CausalR
#' @examples
#' expData<-read.table(system.file(package='CausalR', 'extdata', 'testData.txt'))
#' GetNumberOfPositiveAndNegativeEntries(expData)

GetNumberOfPositiveAndNegativeEntries <- function(dataList) {
    
    
    data <- as.numeric(dataList[, 2])
    dataStats <- c(sum(data == 1), sum(data == -1))
    
    return(dataStats)
} 
