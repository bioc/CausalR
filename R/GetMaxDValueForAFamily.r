#' @title get maximun D value for a family
#' @description
#' Computes the maximum D value for a particular family - denoted as D_fam on page 6 of Assessing Statistical Signifcance of Causal Graphs
#' @param r_p row sum r+
#' @param r_m row sum r-
#' @param c_p column sum c+
#' @param predictionListStats approximate values of n++, n+-, n-+ and n--
#' @param experimentalDataStats a vector containing the values q+, q- and q0:  number of positive, negative, non-significant/contradictory predictions
#' @param logOfFactorialOfPredictionListStats a vector containing the values n+, n- and n0:  number of positive, negative, non-significant/contradictory observations
#' @param returnlog return result as log, default value is FALSE
#' @return the maximum DFam Value


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.



GetMaxDValueForAFamily <- function(r_p, r_m, c_p, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, returnlog = FALSE) {
    # t
    total <- r_p + r_m
    
    # Compute the values of n++, n+-, n-+ and n-- that maximise the D-value; these correspond to the following formula: n_ab is approximately equal to
    # q_a*n_b/T, where T = q+ + q- + q0 = n+ + n- + n0, and a,b are either + or -.  See Assessing statistical significance in causal graphs, page 6.  We
    # need integer values and we need to respect the row and column totals; achieve this by rounding n_pp up (arbitrarily) and calculate the other
    # values to get the right totals.
    if (total > 0) {
        n_pp <- ceiling(r_p * c_p/total)
        n_pm <- r_p - n_pp
        n_mp <- c_p - n_pp
        n_mm <- r_m - n_mp
        maximumDFamValue <- GetApproximateMaximumDValueFromTwoByTwoContingencyTable(n_pp, n_pm, n_mp, n_mm, predictionListStats, experimentalDataStats, 
            logOfFactorialOfPredictionListStats, returnlog)
    } else {
        maximumDFamValue <- GetApproximateMaximumDValueFromTwoByTwoContingencyTable(0, 0, 0, 0, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, 
            returnlog)
    }
    
    return(maximumDFamValue)
} 
