#' @title 
#' analyse experimental data
#' @description
#' Returns the number of up- and down-regulated genes in the experimental data

#' @param  experimentalData  a dataframe containing a list of genes with corresponding direction of change (1 or -1)
#' @return up and down regulation statistics for the experimental data


AnalyseExperimentalData <- function(experimentalData){

  
  experimentalDataStats <- GetNumberOfPositiveAndNegativeEntries(experimentalData)
  experimentalDataStats[3] <- nrow(experimentalData) - sum(experimentalDataStats[1:2])
  
  return(experimentalDataStats)
}