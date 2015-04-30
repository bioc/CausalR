#' @title find approximate values that will maximise D value
#' @description 
#' Finds an approximate table values to maximise D.
#' Given the values of q+, q-, q0, n+, n- and n0 this function will produce the approximate values
#' of n++, n+-, n-+ and n-- that will maximise the D value. See Assessing statistical significance of 
#' casual graphs, page 6. The values are approximate since they need to be rounded, although the direction
#' of rounding is not clear at this stage.
#' 

#' @param predictionListStats a vector containing the values q+, q- and q0: 
#'		 numbers of positive, negative and non-significant/contradictory predictions
#' @param experimentalDataStats a vector containing the values n+, n- and n0:
#'		 numbers of positive, negative and non-significant/contradictory predictions
#' @return a 2x2 contingency table which approximately maximises D


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.

FindApproximateValuesThatWillMaximiseDValue <- function(predictionListStats, experimentalDataStats){


# Inputs:
# predictionListStats			A vector containing the values q+, q- and q0 
#								(the number of positive/negative/non-significant 
#							    (or contradictory) predictions)
# experimentalResultStats 	  	A vector containing the values n+, n- and n0 
#								(the number of positive/negative/non-significant 
#								(or contradictory) transcripts in the results)

q_p <- predictionListStats[1]
q_m <- predictionListStats[2]
q_z <- predictionListStats[3]

n_p <- experimentalDataStats[1]
n_m <- experimentalDataStats[2]
n_z <- experimentalDataStats[3]

T <- sum(predictionListStats) 
# The values of n++, n+-, n-+ and n-- that give the maximum D-value are
# given by the formula within the paper - Assessing statistical significance 
# in causal graphs, page 6.  The formula is n_ab is approximately equal to
# q_a*n_b/T, where T = q+ + q- + q0 = n+ + n- + n0, and a,b are either + or -.
n_pp <- q_p*n_p/T
n_pm <- q_p*n_m/T
n_mp <- q_m*n_p/T
n_mm <- q_m*n_m/T

twoByTwoContingencyTable <- c(n_pp, n_pm, n_mp, n_mm)

return(twoByTwoContingencyTable)
}