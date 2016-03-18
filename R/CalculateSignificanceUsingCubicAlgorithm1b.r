#' @title Calculate Significance Using Cubic Algorithm 
#' @description
#' Calculate the p-value of a score given the hypothesis score and the distribution table (calculated using the cubic algorithm  1b in Assessing statistical significance in causal graphs - Chindelevitch et al)

#' @export
#' @concept CausalR
#' @param hypothesisScore The score whose p-value we want to find.
#' @param predictionListStats Number of predicted up-regulated, 
#'predicted down-regulated and ambiguous predictions.
#' @param experimentalDataStats Number of up-regulated, down-regulated and not 
#' significantly changed transcripts in the experimental data. 
#' @param epsilon  The threshold that is used when calculating the p-value 
#' using the cubic algorithm. (Defaults to 1e-5, only used for the cubic 
#' algorithm, ignored if useCubicAlgorithm is FALSE.)
#' @examples
#' CalculateSignificance(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingQuarticAlgorithm(5, c(7,4,19), c(6,6,18))
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), useCubicAlgorithm=FALSE)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), 1e-5)
#' CalculateSignificance(5, c(7,4,19), c(6,6,18), epsilon=1e-5, useCubicAlgorithm=TRUE)
#' CalculateSignificanceUsingCubicAlgorithm1b(5, c(7,4,19), c(6,6,18), 1e-5)

#' @return  p value

CalculateSignificanceUsingCubicAlgorithm1b <- function(hypothesisScore, predictionListStats, experimentalDataStats, epsilon) {
    
    # Calculate the log of the factorial for all values in predictionListStats, this is to reduce the number of computations done later on.
    logOfFactorialOfPredictionListStats <- lfactorial(predictionListStats)
    
    logDMax <- FindMaximumDValue(predictionListStats, experimentalDataStats, logOfFactorialOfPredictionListStats, TRUE)
    
    # Number of positive predictions from the network. This will be an upper bound for r+ (r_p)
    q_p <- predictionListStats[1]
    # Number of negative predictions from the network. This will be an upper bound for r- (r_m)
    q_m <- predictionListStats[2]
    
    # Number of positive results. This will be an upper bound for c+ (c_p)
    n_p <- experimentalDataStats[1]
    # Number of negative results. This will be an upper bound for c- (c_m)
    n_m <- experimentalDataStats[2]
    
    # Array for the D values - first element is the total D values which have a score equal to or better than the cut-off, second element is all D
    # values (i.e. the ratio is the P value).
    logepsDMax <- log(epsilon) + logDMax
    weights <- c(0, 0)
    
    # In algorithm a superfamily is defined by the left 3x2 submatrix of the contingency table. Now we only need to interate over two quantities: r+ and
    # r-.
    for (r_p in 0:q_p) {
        
        # for 0 to the limit of negative predictions
        for (r_m in 0:q_m) {
            
            # for 0 to the limit of number of positive results Calculate r0
            r_z <- n_p + n_m - (r_p + r_m)
            
            # Need to check that n00 is postive, to ensure it is a valid superfamily
            if (r_z <= predictionListStats[3] && r_z >= 0) {
                
                # The five values in the array below define the superfamily
                weights <- GetWeightsAboveHypothesisScoreForAThreeByTwoTable(weights, r_p, r_m, r_z, n_p, n_m, predictionListStats, experimentalDataStats, 
                    logOfFactorialOfPredictionListStats, hypothesisScore, logepsDMax, logDMax)
            }
        }
    }
    
    pValue <- weights[1]/weights[2]
    
    return(pValue)
} 
