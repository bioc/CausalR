#' @title  validate format of the experimental data table
#' @description
#' Checks the format of the experimental data. This is expected to be two columns, the first containing the gene name and the second the direction of regulation, -1, 0 or 1. The function checks the number of columns and the values of the second column,
#' @param dataTable the data table to be tested 
#' @return true if the data table is valid

ValidateFormatOfDataTable <- function(dataTable){

isValid = TRUE

# Test 1
# Check the table has two columns
if (ncol(dataTable) != 2){
	print("The experimental data read in didn't have two columns")
	isValid = FALSE
}
# Test 2
# Check all the entries in the second column are 1, 0 or -1.
if (isValid){
	for (i in 1:nrow(dataTable)){
		value <-(dataTable[i,2])
		if (!((value=="1")|(value=="0")|(value=="-1")|(value=="+1"))){
		print("The values in the second column do not match what is expected. They should be (+)1, 0 or -1")
		isValid = FALSE
		}
	}
}

return(isValid)
}