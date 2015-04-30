#' @title  check possible values are valid
#' @description
#' Checks if the a given set of possible values for n++, n+-, n-+ and n-- are agree with the predicted and experimental
#' data
#'

#' @param predictionDataStats a vector of predicted results
#' @param experimentalDataStats a vector of observed experimental results
#' @param possibleValues a vector of possible values  n++, n+-, n-+ and n--

#' @return TRUE if and only if the given vector of possible values is valid

CheckPossibleValuesAreValid <- function(predictionDataStats, experimentalDataStats, possibleValues){

numPositivePredictions <- predictionDataStats[1]
numNegativePredictions <- predictionDataStats[2]
numZeroPredictions <- predictionDataStats[3]
numUpregulatedGenes <- experimentalDataStats[1]
numDownregulatedGenes <- experimentalDataStats[2]
numNonResponsiveGenes <- experimentalDataStats[3]

# n_pp = possible value for numCorrectPositivePredictions
n_pp <- possibleValues[1]
# n_pm = possible value for numIncorrectPositivePredictions
n_pm <- possibleValues[2]
# n_mp = possible value for numIncorrectNegativePredictions
n_mp <- possibleValues[3]
# n_mm = possible value for numCorrectNegativePredictions
n_mm <- possibleValues[4]

# The conditions that need to be fulfilled are a series of inequalities
# The conditions will be number so that they can matched to the test function
# Condition 1
if (((n_pp + n_pm) > numPositivePredictions)){
	return(FALSE)
}
# Condition 2
if (((n_mp + n_mm) > numNegativePredictions)){
	return(FALSE)
}
# Condition 3
if (((n_pp + n_mp) > numUpregulatedGenes)){
	return(FALSE)
}
# Condition 4
if (((n_pm + n_mm) > numDownregulatedGenes)){
	return(FALSE)
}

# n_pz (n+0) is the number of genes with positive predictions but are in the set of non-responsive genes in the results.
n_pz <- (numPositivePredictions - n_pp - n_pm)
# Condition 5
if (n_pz >  numNonResponsiveGenes){
	return(FALSE)
}
# n_mz (n-0) is the number of genes with negative predictions but are in the set of non-responsive genes in the results.
n_mz <- (numNegativePredictions - n_mp - n_mm)
# Condition 6
if (n_mz > numNonResponsiveGenes){
	return(FALSE)
}
# n_zp (n0p) is the number of genes predicted to have a non-significant interaction with the hypothesis but are 
# actually in the positively affected in the results 
n_zp <- (numUpregulatedGenes - n_pp - n_mp)
# Condition 7
if (n_zp > numZeroPredictions){
	return(FALSE)
} 
# n_zm (n0m) is the number of genes predicted to have a non-significant interaction with the hypothesis but are 
# actually in the negatively affected in the results 
n_zm <- (numDownregulatedGenes - n_pm - n_mm)
# Condition 8
if (n_zm > numZeroPredictions){
	return(FALSE)
}
# Condition 9
if (((n_pz + n_mz)> numNonResponsiveGenes)){
	return(FALSE)
}
# Condition 10
if (((n_zp + n_zm)> numZeroPredictions)){
	return(FALSE)
}

return(TRUE)
}