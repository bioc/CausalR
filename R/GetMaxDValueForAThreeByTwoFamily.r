#' @title get maximum D value for three-by-two a family
#' @description
#' Returns the maximum D value for a particular family as described as D_fam 
#'  on pages 6 and 7 of  Assessing Statistical Significance of Causal Graphs in Assessing Statistical Signifcance of Causal Graphs
#' @param r_p a r+ row sum from the prediction-observation matrix 
#' @param r_m a  r- row sum from the prediction-observation matrix 
#' @param r_z a  r0 row sum from the prediction-observation matrix 
#' @param n_p a number of predicted increases from the prediction-observation matrix 
#' @param n_m a number of predicted decreases  from the prediction-observation matrix 
#' @param predictionListStats a vector contain the number of postive, negative and non-significant/contradictory predictions: q+, q- and q0.
#' @param logOfFactorialOfPredictionListStats a vector containing the log of the factorial for each element in the predictionListStats object
#' @param returnlog whether or not the maximum D value should be returned as a log (TRUE). Otherwise a non-logged value is returned.
#' @return Maximum D_fam Value


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.


GetMaxDValueForAThreeByTwoFamily <- function(r_p, r_m, r_z, n_p, n_m, predictionListStats,  
                                             logOfFactorialOfPredictionListStats, returnlog = FALSE){
  # Compute the maximum D value for a particular
  # superfamily (a 3x2 matrix) - described in Assessing Statistical Significance of Causal Graphs (page 6/7)
  
  # Inputs:
  # twoByTwoContingencyTable		Approximate values of n++, n+-, n-+ and n--, these values are
  #			statistical significance of causal graphs)
  # predictionListStats		A vector containing the values q+, q- and q0 
  #			(the number of positive/negative/non-significant 
  #			(or contradictory) predictions)
  # logOfFactorialOfPredictionListStats 	A vector containing the log of the factorial value for
  #			each entry in predictionListStats
  # returnlog			A boolean describing whether the log of the D-Value should
  #			be returned
  
  total <- n_p + n_m
  
  # Compute the values of n++, n+-, n-+, n--, n0+, and n0- that maximise the D-value; 
  # these correspond to the following formula: n_ab is approximately equal to
  # q_a*n_b/T, where T = n+ + n-, and a,b are either + or -.
  # See  Assessing statistical significance in causal graphs, page 7 - the formula is 
  # not stated explicitly but follows from the logic of algorithm 1a).
  if (total > 0){
    n_pp <- ceiling(r_p*n_p/total)
    n_pm <- r_p - n_pp
    n_mp <- ceiling(r_m*n_p/total)
    # Check this rounding produces a valid combination i.e. n++ + n-+ <= n+
    if ((n_mp + n_pp) > n_p){
      n_mp <- floor(r_m*n_p/total)
    }
    n_mm <- r_m - n_mp 
    n_zp <- n_p - (n_pp + n_mp)
    n_zm <- n_m - (n_pm + n_mm)
  }
  else{
    n_pp <- 0
    n_pm <- 0
    n_mp <- 0
    n_mm <- 0
    n_zp <- 0
    n_zm <- 0
  }
  
  threeByTwoContingencyTable <- c(n_pp,n_pm,n_mp,n_mm, n_zp, n_zm)
  
  maximumDFamValue <- GetApproximateMaximumDValueFromThreeByTwoContingencyTable(threeByTwoContingencyTable, predictionListStats, logOfFactorialOfPredictionListStats, returnlog)
  
  return(maximumDFamValue)
}