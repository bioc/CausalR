#' @title get score for numbers of correct and incorrect predictions
#' @description
#' Returns all possible rounding combinations of a 2x2 table. 
#' Given the values of n++, n+-, n-+ and n-- (stored in twoByTwoContingencyTable) this function will
#' compute all possibilities of rounding each value up or down. 
#' 
#' @param twoByTwoContingencyTable    Approximate values of n++, n+-, n-+ and n--, these values are
#'								calculated to optimise the D-value (see page 6 of Assessing
#'								statistical significance of causal graphs)
#' @return a matrix of rounding combinations

GetAllPossibleRoundingCombinations <- function(twoByTwoContingencyTable){
  # Given the values of n++, n+-, n-+ and n-- (stored in twoByTwoContingencyTable) this function will
  # compute all possibilities of rounding each value up or down. 
  #
  # Inputs:
  # twoByTwoContingencyTable  	Approximate values of n++, n+-, n-+ and n--, these values are
  #								calculated to optimise the D-value (see page 6 of Assessing
  #								statistical significance of causal graphs)

  
  f_n_pp <- floor(twoByTwoContingencyTable[1])
  f_n_pm <- floor(twoByTwoContingencyTable[2])
  f_n_mp <- floor(twoByTwoContingencyTable[3])
  f_n_mm <- floor(twoByTwoContingencyTable[4])
  
  c_n_pp <- ceiling(twoByTwoContingencyTable[1])
  c_n_pm <- ceiling(twoByTwoContingencyTable[2])
  c_n_mp <- ceiling(twoByTwoContingencyTable[3])
  c_n_mm <- ceiling(twoByTwoContingencyTable[4])
  
  roundingCombinations <- matrix(c(f_n_pp, f_n_pm, f_n_mp, f_n_mm, 
                                   f_n_pp, f_n_pm, f_n_mp, c_n_mm, 
                                   f_n_pp, f_n_pm, c_n_mp, f_n_mm, 
                                   f_n_pp, f_n_pm, c_n_mp, c_n_mm, 
                                   f_n_pp, c_n_pm, f_n_mp, f_n_mm, 
                                   f_n_pp, c_n_pm, f_n_mp, c_n_mm, 
                                   f_n_pp, c_n_pm, c_n_mp, f_n_mm, 
                                   f_n_pp, c_n_pm, c_n_mp, c_n_mm, 
                                   c_n_pp, f_n_pm, f_n_mp, f_n_mm, 
                                   c_n_pp, f_n_pm, f_n_mp, c_n_mm, 
                                   c_n_pp, f_n_pm, c_n_mp, f_n_mm, 
                                   c_n_pp, f_n_pm, c_n_mp, c_n_mm, 
                                   c_n_pp, c_n_pm, f_n_mp, f_n_mm, 
                                   c_n_pp, c_n_pm, f_n_mp, c_n_mm, 
                                   c_n_pp, c_n_pm, c_n_mp, f_n_mm, 
                                   c_n_pp, c_n_pm, c_n_mp, c_n_mm), ncol=4, byrow=TRUE)
  
  
  # If any of the input values are integers then there will be duplicates in the list
  # Remove the duplicates
  keep = array(TRUE,16)
  if (f_n_pp == c_n_pp){
    keep[9:16] <- FALSE
  }
  if (f_n_pm == c_n_pm){
    keep[c(5:8,13:16)] <- FALSE
  }
  if (f_n_mp == c_n_mp){
    keep[c(3:4,7:8,11:12,15:16)] <- FALSE
  }
  if (f_n_mm == c_n_mm){
    keep[c(2,4,6,8,10,12,14,16)] <- FALSE
  }
  
  return(roundingCombinations[keep,])
}