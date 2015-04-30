#' @title populate the three-by-three contingency table 
#' @description
#' Populates 3x3 signed contingency table of expected versus observed changes.
#' Given the values of n++, n+-, n-+ and n--, calculates n0+, n0-, n+0, n-0 and n00. 
#' Notation from Chindelevitch et al. Causal reasoning on biological networks 
#' Bioinformatics (2012) paper.

#' @param n_pp n++ contingency table entry
#' @param n_pm n+- contingency table entry
#' @param n_mp n-+ contingency table entry
#' @param n_mm n-- contingency table entry
#' @param predictionDataStats a prediction data table.
#' @param experimentalDataStats an experimental data table 
#' @return Vector of calculated values for n0+, n0-, n+0, n-0 and n00 - See: Chindelevitch et al.Bioinformatics (2012).


PopulateTheThreeByThreeContingencyTable <- function(n_pp, n_pm, n_mp, n_mm, predictionDataStats, experimentalDataStats){
  
  n_pz <- predictionDataStats[1] - n_pp - n_pm
  n_mz <- predictionDataStats[2] - n_mp - n_mm
  
  return(c(n_pp, n_pm, n_pz, 
           n_mp, n_mm, n_mz, 
           (experimentalDataStats[1] - n_pp - n_mp), (experimentalDataStats[2] - n_pm - n_mm),(experimentalDataStats[3] - n_pz - n_mz)))
  
}

#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.