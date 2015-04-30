#' @title updates weights for contingency table and produce values for p-value calculation
#' @description
#' Finds the D-Values (weights) from any 3x2 contingency tables 
#' that have a score above and including the hypothesis score. It also calculates the
#' total weight, and returns a 2x1 vector of the two values. The ratio of these values is the p-value.

#' @param weights Weights
#' @param r_p the row sum r+
#' @param r_m the row sum r-
#' @param r_z the row sum r0
#' @param n_p the column sum n+
#' @param n_m the column sum n-	
#' @param predictionListStats a list of prediction statistics
#' @param experimentalDataStats the observed experimental data
#' @param logOfFactorialOfPredictionListStats log factorial's of prediction list stats
#' @param hypothesisScore the hypothesis score to be considered
#' @param logepsDMax log of epsilon logD Maximum value
#' @param logDMax a logD Maximum value
#' @return a vector containing the hypothesis score and the total weight

GetWeightsAboveHypothesisScoreForAThreeByTwoTable<- function(weights, r_p, r_m, r_z, n_p, n_m, 
                                                             predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, 
                                                             hypothesisScore, logepsDMax, logDMax){
  
  # To reduce the overall runtime of the algorithm, the approximate maxDValue for the family is calculated, rather than the actual.  
  log_approxDFamilyMax <- GetMaxDValueForAThreeByTwoFamily(r_p, r_m, r_z, n_p, n_m, predictionListStats, 
                                                           logOfFactorialOfPredictionListStats, TRUE)
  if (log_approxDFamilyMax > logepsDMax){
    # Calculate the values of n++ and n-+ that were used to calculate approxDFamilyMax
    
    # t = n_p + n_m ( the sum of all the values in the 3x2 submatrix)
    total <- n_p + n_m

    if (total > 0){
      n_pp <- ceiling(r_p*n_p/total)
    }
    else{
      n_pp <- 0
    }
      
    # Iterate over possible values of n++, starting from the one calculated above.  
    # Since the values decrease monotonically either side of this value, we start at this value and 
    # iterate in both directions until the D value is less than a given threshold. This will give
    # us the non-neglible parts of the distribution.
 
    # An upper bound for possible values of n++ is the minimum of r+ and n+
    for (m_pp in n_pp:min(r_p,n_p)){
      
      # We still need to define one more value in the 3x2 matrix to determine all the rest 
      # n+- is bounded by the minumum of r- and n+
      for (m_mp in 0:min(r_m,n_p)){
        
        #n+-
        m_pm <- r_p - m_pp
        #n--
        m_mm <- r_m - m_mp
        #n0+
        m_zp <- n_p - (m_pp + m_mp)
        #n0-
        m_zm <- n_m - (m_pm + m_mm)
        contingencyTableValues <- c(m_pp, m_pm, m_mp, m_mm, m_zp, m_zm)
        
        m_pz <- predictionListStats[1] - r_p
        m_mz <- predictionListStats[2] - r_m
        m_zz <- predictionListStats[3] - r_z
        
        # None of these values can be less than zero
        if (min(contingencyTableValues)>= 0){
        
          threeByThreeContingencyTable <- c(m_pp, m_pm, m_pz, m_mp, m_mm, m_mz, m_zp, m_zm, m_zz)
          logDValue <- sum(logOfFactorialOfPredictionListStats) - sum(lfactorial(threeByThreeContingencyTable))
          # Only need to compute the score if the D-value is non-neglible   			             
          if (logDValue > logepsDMax){
            weight_contrib <- exp(logDValue - logDMax)
            weights[2] <- weights[2] + weight_contrib
            
            # Rather than just calculate the DValue by taking the exponential of logDvalue, we first subtract a constant.
            # This means when we take the exponential we get the DValue divided by a different constant, but this cancels out
            # when we take ratios later when computing the p-value
            if (m_pp + m_mm - (m_pm + m_mp) >= hypothesisScore){
              weights[1] <- weights[1] + weight_contrib
            }
          }   
        }
      }		
    }
    
    # The loop below does exactly the same thing as the one above; it covers the cases that
    # are not done in the loop above. A lower bound for possible values of n++ is 0
    # To avoid n_pp become negative or double counting when n_pp = 0, we add the following if statement
    if (n_pp > 0){
      for (m_pp in (n_pp-1):0){
      
        # Iterate over all values of m_mp 
        for (m_mp in 0:min(r_m,n_p)){

          #n+-
          m_pm <- r_p - m_pp
          #n--
          m_mm <- r_m - m_mp
          #n0+
          m_zp <- n_p - (m_pp + m_mp)
          #n0-
          m_zm <- n_m - (m_pm + m_mm)
          contingencyTableValues <- c(m_pp, m_pm, m_mp, m_mm, m_zp, m_zm)
          
          m_pz <- predictionListStats[1] - r_p
          m_mz <- predictionListStats[2] - r_m
          m_zz <- predictionListStats[3] - r_z
          
          # None of these values can be less than zero
          if (min(contingencyTableValues)>= 0){

            threeByThreeContingencyTable <- c(m_pp, m_pm, m_pz, m_mp, m_mm, m_mz, m_zp, m_zm, m_zz)
            logDValue <- sum(logOfFactorialOfPredictionListStats) - sum(lfactorial(threeByThreeContingencyTable))
            
            # Only need to compute the score if the D-value is non-neglible 
            if (logDValue > logepsDMax){
              # Rather than just calculate the DValue by taking the exponential of logDvalue, we first subtract a constant.
              # This means when we take the exponential we get the DValue divided by a different constant, but this cancels out
              # when we take ratios later when computing the p-value
              weight_contrib <- exp(logDValue - logDMax)
              weights[2] <- weights[2] + weight_contrib
              if (m_pp + m_mm - (m_pm + m_mp) >= hypothesisScore){
                  weights[1] <- weights[1] + weight_contrib
              }
            }	  
          }
        }
      }	
    }
  }
  return(weights) 	
}
