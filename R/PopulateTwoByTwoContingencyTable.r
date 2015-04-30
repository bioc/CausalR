#' @title Populate Two by Two Contingency Table
#' @description
#' Calculates a 2x2 contingency table.
#' Given the value of n++ and the row and column sums (r+, r-, c+, c-),
#' Calculates the remaining values in the 2x2 contingency table i.e.
#' n+-, n-+, and n--. See Chindelevich et al. BMC Bioinformatics (2012) paper 
#' 'Assessing Statistical significance of causal graphs' for clarification on notation.

#' @param rowAndColumnSumValues the row and column sums (r+, r-, c+, c-).
#' @param n_pp the value of n++.
#' @return  the completed 2x2 contingency table: n++, n+-, n-+, n--

#' @references
#' L Chindelevitch et al.
#' Causal reasoning on biological networks: Interpreting transcriptional changes.
#' Bioinformatics, 28(8):1114-21, 2012.   

PopulateTwoByTwoContingencyTable <- function(rowAndColumnSumValues,n_pp){

# r+
r_p <- rowAndColumnSumValues[1]
# r-
r_m <- rowAndColumnSumValues[2]
# c+
c_p <- rowAndColumnSumValues[3]
# c-
c_m <- rowAndColumnSumValues[4]

#n+-
n_pm <- r_p - n_pp
#n-+
n_mp <- c_p - n_pp
#n--
n_mm <- r_m - n_mp

contingencyTableValues <- c(n_pp, n_pm, n_mp, n_mm)
return(contingencyTableValues)
}