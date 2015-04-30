#' @title read .sif to Table
#' @description
#' Reads a .sif file into a table in R
#' @param sifFile the sifFile to be read in 
#' @return a R table containing the data from the .sif file

ReadSifFileToTable <- function(sifFile){

table <- read.table(sifFile, colClasses = "character")

ValidateFormatOfTable(table)

return(table) 
}