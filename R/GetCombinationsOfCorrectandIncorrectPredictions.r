#' @title returns table of correct and incorrect predictions
#' @description
#' Returns the  numbers of correct and incorrect positive and negative predictions
#' 
#' @param predictionDataStats prediction data statistics table
#' @param experimentalDataStats Experimental data statistics table
#' @return a matrix the  numbers of correct and incorrect positive and negative prediction 
#' 


GetCombinationsOfCorrectandIncorrectPredictions<- function(predictionDataStats, experimentalDataStats){

numPositivePredictions <- predictionDataStats[1]
numNegativePredictions <- predictionDataStats[2]
numUpregulatedGenes <- experimentalDataStats[1]
numDownregulatedGenes <- experimentalDataStats[2]

# First find bounds for numCorrectPositivePredictions, numIncorrectPositivePredictions, numCorrectNegativePredictions
# and numIncorrectNegativePredictions
boundForNumCorrectPositivePredictions <- min(numPositivePredictions,numUpregulatedGenes)
boundForNumIncorrectPositivePredictions <- min(numPositivePredictions,numDownregulatedGenes)
boundForNumIncorrectNegativePredictions <- min(numNegativePredictions,numUpregulatedGenes) 
boundForNumCorrectNegativePredictions <- min(numNegativePredictions,numDownregulatedGenes) 

# NOTE: Incorrect predictions are those that are predicted positive, but in the results turn out to be negative or
# vice versa

maxNumberOfRows <- (boundForNumCorrectPositivePredictions+1)*(boundForNumIncorrectPositivePredictions+1)*(boundForNumIncorrectNegativePredictions+1)*(boundForNumCorrectNegativePredictions+1)
matrixOfPossibleValues <- matrix(0,maxNumberOfRows,4)

# Let n_pp = possible value for numCorrectPositivePredictions
#	  n_pm = possible value for numIncorrectPositivePredictions
#     n_mp = possible value for numIncorrectNegativePredictions
#     n_mm = possible value for numCorrectNegativePredictions
# (p is used to replace the + and m to replace the -)	

# Iterate over possible values of n++, n+-, n-+ and n--  and check if they are valid
counter = 1
for (n_pp in 0:boundForNumCorrectPositivePredictions){
	for (n_pm in 0:boundForNumIncorrectPositivePredictions){
		for (n_mp in 0:boundForNumIncorrectNegativePredictions){
			for (n_mm in 0:boundForNumCorrectNegativePredictions){
				possibleValues <- c(n_pp, n_pm, n_mp, n_mm)
				if(CheckPossibleValuesAreValid(predictionDataStats, experimentalDataStats, possibleValues)){
					matrixOfPossibleValues[counter,] <- possibleValues
					counter = counter + 1
				}
			}
		}
	}
}
matrixOfPossibleValues <- matrixOfPossibleValues[1:counter-1,]
return(matrixOfPossibleValues)			
}