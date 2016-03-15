#' @title validate format of table
#' @description 
#' Checks the format of the loaded in data. In particular exepects a table with threecolumns (in order) a initiating gene, an interaction ('Activates','Inhibits') and a responding gene and checks the number of rows and the values of the middle column.
#' @param dataTable the table to be tested
#' @return true if the test is satisfed.

ValidateFormatOfTable <- function(dataTable) {
    
    # The table should have three columns: the initiating gene, the type of interaction, and the responding gene
    if (!dim(dataTable)[2] == 3) {
        stop("The number of columns in the input file is incorrect")
    }
    
    # Test 2 The type of interaction column should either contain the entry 'Activates' or 'Inhibits'
    for (counter in 1:dim(dataTable)[1]) {
        if (!((dataTable[[2]][counter] == "Activates") || (dataTable[[2]][counter] == "Activation") || (dataTable[[2]][counter] == "Inhibits") || (dataTable[[2]][counter] == 
            "Inhibition"))) {
            stop("The middle column of the input file is incorrect. Each entry should be 'Activates/Activation' or 'Inhibits/Inhibition'")
        }
    }
} 
