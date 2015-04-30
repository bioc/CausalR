#' @title check row and column sum values are valid
#' @description
#' Checkes to see if the values of r+, r-, c+ and c- which are stored in rowAndColumnSumValues define a valid contingency
#' table
#' @param rowAndColumnSumValues  a 4x1 vector containing the row and column sum values (r+, r-, c+, c-) for a 2x2 contingency table
#' @param predictionListStats	a vector containing the values q+, q- and q0 
#' @param experimentalResultStats A vector containing the values n+, n- and n0
#' @return TRUE if the table is valid; otherwise FALSE


  # Check if the values of r+, r-, c+ and
  # c- which are stored in rowAndColumnSumValues make up a valid contingency
  # table in relation to the whole 3x3 table
  
  # Inputs:
  # rowAndColumnSumValues  		A 4x1 vector containing the row and column sum values (r+, r-, c+, c-) 
  #								for a 2x2 contingency table
  # predictionListStats			A vector containing the values q+, q- and q0 
  #								(the number of positive/negative/non-significant 
  #							    (or contradictory) predictions)
  # experimentalResultStats 	  	A vector containing the values n+, n- and n0 
  #								(the number of positive/negative/non-significant 
  #								(or contradictory) transcripts in the results)
  

  
CheckRowAndColumnSumValuesAreValid <- function(rowAndColumnSumValues, predictionListStats, experimentalResultStats){
  
  # r+ and r-
  r_p <- rowAndColumnSumValues[1]
  r_m <- rowAndColumnSumValues[2]
  # c+ and c-
  c_p <- rowAndColumnSumValues[3] 
  c_m <- rowAndColumnSumValues[4]
  
  if (c_m < 0){
    return(FALSE)
  }
  
  # q+, q- and q0 
  q_p <- predictionListStats[1]
  q_m <- predictionListStats[2]
  q_z <- predictionListStats[3]
  
  # n+, n- and n0
  n_p <- experimentalResultStats[1]
  n_m <- experimentalResultStats[2]
  n_z <- experimentalResultStats[3]
  
  # Compute the other values in the 3x3 table
  
  n_pz <- q_p - r_p
  if (n_pz < 0){
    return(FALSE)
  }
  
  n_mz <- q_m - r_m
  if (n_mz < 0){
    return(FALSE)
  }
  
  n_zp <- n_p - c_p
  if (n_zp < 0){
    return(FALSE)
  }
  
  n_zm <- n_m - c_m
  if (n_zm < 0){
    return(FALSE)
  }
  
  n_zz <- q_z - (n_zp + n_zm)
  
  if(n_zz <0){
    return(FALSE)
  }
  
  return(TRUE)
}