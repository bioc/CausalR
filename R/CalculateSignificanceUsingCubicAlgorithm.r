#' @title calculate significance using the cubic algorithm 
#' @description
#' Calculates the p-value of a score given the hypothesis score and the distribution table (calculated using the cubic algorithm)
#' @export
#' @param  hypothesisScore the score whose p-value we want to find.
#' @param  predictionListStats numbers of predicted up-regulated, predicted down-regulated and ambiguous predictions.
#' @param  experimentalDataStats numbers of up-regulated, down-regulated and not significantly changed transcripts in the experimental data. 
#' @param  epsilon an epsilon threshold that is used when calculating the p-value 
#' using the cubic algorithm. Defaults to 1e-5.
#' @return  p-value
#' @examples
#' CalculateSignificance(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingQuarticAlgorithm(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=FALSE)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), 1e-5)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), epsilon=1e-5, useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingCubicAlgorithm(5, c(7,4,19), c(6,6,18), 1e-5)

#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.




CalculateSignificanceUsingCubicAlgorithm <- function(hypothesisScore, predictionListStats, experimentalDataStats, epsilon){
    
  # Calculate the log of the factorial for all values in predictionListStats, this is to 
  # reduce the number of computations done later on. 
  logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
  
  logDMax <- FindMaximumDValue(predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, TRUE)
  
  # Not all values of r_, r- c+, c- are possible for the contigency table. This step finds all that combinations
  # that are valid. Each row of this matrix will represent a family of contingency tables.
  # Number of positive predictions from the network. This will be an upper bound for r+ (r_p)
  q_p <- predictionListStats[1]
  # Number of negative predictions from the network. This will be an upper bound for r- (r_m)
  q_m <- predictionListStats[2]
  # Number of positive results. This will be an upper bound for c+ (c_p)
  n_p <- experimentalDataStats[1]
  # Number of negative results. This will be an upper bound for c- (c_m)
  n_m <- experimentalDataStats[2]
  
  r_pm_limit <- n_p + n_m - predictionListStats[3]
  
  # Array for the D values - first element is the total D values which have a score equal to or better
  # than the cut-off, second element is all D values (i.e. the ratio is the P value).
  weights <- c(0,0)
  logepsDMax <- log(epsilon) + logDMax
  
  # Iterate all possible values of r+, r-, c+ and c- and check if they produce a valid contingency table
  # If so, calculate how much these values contribute to the weights.
  for (r_p in 0:q_p){
    r_m_min <- max(0,r_pm_limit - r_p)
    if (r_m_min <= q_m){
      for (r_m in r_m_min:q_m){
        c_p_min <- max(0,r_p + r_m - n_m)
        c_p_max <- min(n_p,r_p + r_m)
        if (c_p_min <= c_p_max){
          for (c_p in c_p_min:c_p_max){
            # calculate the column sum of m
            weights <- weights + GetWeightsAboveHypothesisScoreAndTotalWeights(r_p, r_m, c_p, predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, hypothesisScore, logepsDMax, logDMax)
          }
        }
      }
    }
  }
  
  pValue <- weights[1]/weights[2]
    
  return(pValue)
}
