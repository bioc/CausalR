#' @title  get weights above hypothesis score and total weights
#' @description 
#' Gets the score based on the values of n++, n+-, n-+ and n--. Used as part of a p-value calculation.
#' @param r_p the row sum r+
#' @param r_m the row sum r-
#' @param c_p the column sum c+
#' @param predictionListStats statistics for the  prediction list
#' @param experimentalDataStats statistics for the experimental data
#' @param logOfFactorialOfPredictionListStats log of factorial of prediction list stats
#' @param hypothesisScore the hypothesis score to be considered
#' @param logepsDMax Exponential of logD Maximum value 
#' @param logDMax A logD Maximum value
#' @return score data




GetWeightsAboveHypothesisScoreAndTotalWeights <- function(r_p, r_m, c_p, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, hypothesisScore, logepsDMax, logDMax){

  
  weights <- c(0,0)
  
  # To reduce the overall runtime of the algorithm, the approximate maxDValue for the family is calculated, rather than the actual.  
  log_approxDFamilyMax <- GetMaxDValueForAFamily(r_p, r_m, c_p, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, TRUE)
  if (log_approxDFamilyMax > logepsDMax){
    # Calculate the value of n++ that is used to calculate approxDFamilyMax
    # t
    total <- r_p + r_m
    
    if (total > 0){
      n_pp <- round(r_p*c_p/total)
    }
    else{
      n_pp <- 0
    }
    
    # Iterate over possible values of n++, starting from the one calculated above.  
    # Since the values decrease monotonically either side of this value, we start at this value and 
    # iterate in both directions until the D value is less than a given threshold. This will give
    # us the non-neglible parts of the distribution.
    
    # An upper bound for possible values of n++ is the minimum of r+ and c+
    for (m_pp in n_pp:min(r_p,c_p)){
      #n+-
      if (r_p >= m_pp){
        m_pm <- r_p - m_pp
        #n-+
        if (c_p >= m_pp){
          m_mp <- c_p - m_pp
          #n--
          if (r_m >= m_mp){
            m_mm <- r_m - m_mp
            logDValue <- GetWeightForNumbersOfCorrectandIncorrectPredictions(m_pp, m_pm, m_mp, m_mm, predictionListStats, experimentalDataStats,
                                                                             logOfFactorialOfPredictionListStats, TRUE)
            if (logDValue > logepsDMax){
              # Rather than just calculate the DValue by taking the exponential of logDvalue, we first subtract a constant.
              # This means when we take the exponential we get the DValue divided by a different constant, but this cancels out
              # when we take ratios later when computing the p-value
              weight_contrib <- exp(logDValue - logDMax)
              weights[2] <- weights[2] + weight_contrib
              if (m_pp + m_mm - m_pm - m_mp  >= hypothesisScore){
                weights[1] <- weights[1] + weight_contrib
              }
            }
            else{
              break
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
        #n+-
        if (r_p >= m_pp){
          m_pm <- r_p - m_pp
          #n-+
          if (c_p >= m_pp){
            m_mp <- c_p - m_pp
            #n--
            if (r_m >= m_mp){
              m_mm <- r_m - m_mp
              logDValue <- GetWeightForNumbersOfCorrectandIncorrectPredictions(m_pp, m_pm, m_mp, m_mm, predictionListStats, experimentalDataStats, 
                                                                               logOfFactorialOfPredictionListStats, TRUE)
              if (logDValue > logepsDMax){
                # Rather than just calculate the DValue by taking the exponential of logDvalue, we first subtract a constant.
                # This means when we take the exponential we get the DValue divided by a different constant, but this cancels out
                # when we take ratios later when computing the p-value
                weight_contrib <- exp(logDValue - logDMax)
                weights[2] <- weights[2] + weight_contrib
                if (m_pp + m_mm - m_pm - m_mp >= hypothesisScore){
                  weights[1] <- weights[1] + weight_contrib
                }
              }
              else{
                break
              }
            }
          }
        }
      }
    }
  }
  
  return(weights)
  
}
