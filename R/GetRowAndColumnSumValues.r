#' @title  get row and column sum values
#' @description
#' Returns the possible values of r+, r-, c+ and c- (the column and row sum values) following page 6 of Assessing
#' statistical  significance in causal graphs (Chindelevitch et. al)
#' @param predictionListStats a vector containing the number of postive, negative, or non-signficant/contradictory predictions (q+, q- and q0)
#' @param experimentalResultStats a vector containing the number of postive, negative, or non-signficant/contradictory observations (n+, n- and n0)
#' @return a matrix of row and sum values r+, r-, c+ and c- 


#' @references
#' L Chindelevitch et al.
#' Assessing statistical significance in causal graphs.
#' BMC Bioinformatics, 13(35), 2012.



GetRowAndColumnSumValues <- function(predictionListStats, experimentalResultStats){


# Number of positive predictions from the network. This will be an upper bound for r+ (r_p)
q_p <- predictionListStats[1]
# Number of negative predictions from the network. This will be an upper bound for r- (r_m)
q_m <- predictionListStats[2]
# Number of positive results. This will be an upper bound for c+ (c_p)
n_p <- experimentalResultStats[1]
# Number of negative results. This will be an upper bound for c- (c_m)
n_m <- experimentalResultStats[2]

# Pre-declare an array to store the valid column/row sum values - since we only iterate over three quantities, the array can have
# a maximum size equal to the product of the three smallest values out of q_p+1, q_m+1, n_p+1, and n_m+1.
maximumValue = max(q_p+1, q_m+1, n_p+1, n_m+1)
maxNumberOfRows <- (q_p+1)*(q_m+1)*(n_p+1)*(n_m+1)/maximumValue
tryCatch({
	possibleRowAndColumnSumValues <- matrix(0,maxNumberOfRows,4)
	# Iterate all possible values of r+, r-, c+ and c- and check if they produce a valid contingency table
	counter <- 1
	for (r_p in 0:q_p){
		# for 0 to the limit of negative predictions
		for (r_m in 0:q_m){
			# for 0 to the limit of number of positive results
			for (c_p in 0:n_p){
				# calculate the column sum of m
				c_m <- r_p + r_m - c_p
				rowAndColumnSumValues <- c(r_p, r_m, c_p, c_m)
				if (CheckRowAndColumnSumValuesAreValid(rowAndColumnSumValues, predictionListStats, experimentalResultStats)){
					if (counter > maxNumberOfRows){
						# double the size of the matrix if it doesn't have any space
						possibleRowAndColumnSumValues <- matrix(c(possibleRowAndColumnSumValues, matrix(0,maxNumberOfRows,4) ), ncol =4)
					}
					
					possibleRowAndColumnSumValues[counter,] <-  rowAndColumnSumValues
					counter <- counter + 1
				}
			}
		}
	}

	# Only want to keep the rows that were populated in the set of for loops above
	possibleRowAndColumnSumValues <- possibleRowAndColumnSumValues[1:counter-1,]

	return(possibleRowAndColumnSumValues)
	}
,error = function(e){
  # If there is an error, report back all valuable information to the user - this is deisgned for when there is a memory error.
  print(e)
  print(paste("Number of positive predictions:", predictionListStats[1]))
  print(paste("Number of negative predictions:", predictionListStats[2]))
  print(paste("Number of ambiguous/unknown predictions:", predictionListStats[3]))
  print(paste("Number of upregualted results:", experimentalResultStats[1]))
  print(paste("Number of down-regualted results:", experimentalResultStats[2]))
  print(paste("Number of non-significantly changed results:", experimentalResultStats[3]))
  print(paste("Number of rows in array to store all the combinations of row and column sums in upper 2x2 submatrix of contingency table:", maxNumberOfRows))
  stop()  
  }
)
}

