#' @title calculate total weight for all contingency tables
#' @description
#' Calculates the total weights or D-values for all possible  contingency tables. This value can be  used to calculate the p-value

#' @param experimentalDataStats a vector containing the values n+, n- and n0, the number of positive/negative/non-significant (or contradictory) transcripts in the results
#' @param returnlog whether the result should be returned as a log. Default is FALSE.

#' @return a D-value or weight

CalculateTotalWeightForAllContingencyTables<-function(experimentalDataStats, returnlog = FALSE){
  

  # Total weight is (total number of measured transcripts)!/((number of positive results)!*(number of negative results)!*(number of non-sig. results)!)
  logOfTotalWeight <- lfactorial(sum(experimentalDataStats)) - (lfactorial(experimentalDataStats[1])  + lfactorial(experimentalDataStats[2])
		+ lfactorial(experimentalDataStats[3]))
		
  if (returnlog){
      return(logOfTotalWeight)
  }
  else{
      return(exp(logOfTotalWeight))
  }  
}