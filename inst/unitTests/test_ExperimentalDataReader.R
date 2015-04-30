# Test suite for the ReadExperimentalData function

library(igraph)
library(CausalR)



test_ValidateFormatOfDataTableReturnsFalseWhenTheInputFileOnlyHasOneColumn <- function(){ 
  matrix1<- matrix(c("node0","node1","node2"), nrow =3)
  dataTableWithOneColumn <- as.table(matrix1)
  checkTrue(!CausalR:::ValidateFormatOfDataTable(dataTableWithOneColumn))
}

test_ValidateFormatOfDataTableReturnsFalseWhenFormatOfTheSecondColumnIsNotCorrect <- function(){ 
  matrix1<- matrix(c("node0","node1","node2", "+", "1", "0"), nrow =3)
  dataTableWithIncorrectSecondColumn <- as.table(matrix1)
  checkTrue(!CausalR:::ValidateFormatOfDataTable(dataTableWithIncorrectSecondColumn))
}

test_ProcessExperimentalDataProducesTheCorrectOutputMatrixWithMatchedInputSize <- function(){
  network <- graph.formula( "node0" -+ "node1", "node0" -+ "node2")
  network$isCCG <- FALSE
  experimentalResults <- matrix(c("node0","node1","node2", "+1", "-1", "1"), nrow =3)
  processedData <- CausalR:::ProcessExperimentalData(experimentalResults, network)
  checkEquals(nrow(processedData),3)
  checkEquals(ncol(processedData),2)
  checkEquals(typeof(processedData),"double")
  checkEquals(processedData[1,1],1)
  checkEquals(processedData[2,1],2)
  checkEquals(processedData[3,1],3)
  checkEquals(processedData[1,2],1)
  checkEquals(processedData[2,2],-1)
  checkEquals(processedData[3,2],1)
}

test_ProcessExperimentalDataProducesTheCorrectOutputMatrixWithSmallerExperimentalResults <- function(){
  network <- graph.formula( "node0" -+ "node1", "node0" -+ "node2")
  network$isCCG <- FALSE
  experimentalResults <- matrix(c("node0","node1", "+1", "-1"), nrow =2)
  processedData <- CausalR:::ProcessExperimentalData(experimentalResults, network)
  checkEquals(nrow(processedData),2)
  checkEquals(ncol(processedData),2)
  checkEquals(typeof(processedData),"double")
  checkEquals(processedData[1,1],1)
  checkEquals(processedData[2,1],2)
  checkEquals(processedData[1,2],1)
  checkEquals(processedData[2,2],-1)
  # Note that this test produces a warning due to the unequal sizes of the inputs
}

test_ProcessExperimentalDataProducesTheCorrectOutputMatrixForCCG <- function(){
  network <- graph.formula( "node0+" -+ "node1+", "node0+" -+ "node2-", "node0-" -+ "node1-", "node0-" -+ "node2+")
  network$isCCG <- TRUE
  V(network)$unsignedName <- c("node0", "node1", "node2", "node0", "node1", "node2")
  experimentalResults <- matrix(c("node0","node1", "+1", "-1"), nrow =2)
  processedData <- CausalR:::ProcessExperimentalData(experimentalResults, network)
  checkEquals(nrow(processedData),2)
  checkEquals(ncol(processedData),2)
  checkEquals(typeof(processedData),"double")
  checkEquals(processedData[1,1],1)
  checkEquals(processedData[2,1],2)
  checkEquals(processedData[1,2],1)
  checkEquals(processedData[2,2],-1)
}


test_ProcessExperimentalDataProducesTheCorrectOutputMatrixWithCCG <- function(){
  network <- graph.formula( "node0+" -+ "node1+", "node0+" -+ "node2-", "node0-" -+ "node1-", "node0-" -+ "node2+")
  network$isCCG <- TRUE
  V(network)$unsignedName <- c("node0", "node1", "node2", "node0", "node1", "node2")
  experimentalResults <- matrix(c("node0","node1","node2", "+1", "-1", "1"), nrow =3)
  processedData <- CausalR:::ProcessExperimentalData(experimentalResults, network)
  checkEquals(nrow(processedData),3)
  checkEquals(ncol(processedData),2)
  checkEquals(typeof(processedData),"double")
  checkEquals(processedData[1,1],1)
  checkEquals(processedData[2,1],2)
  checkEquals(processedData[3,1],3)
  checkEquals(processedData[1,2],1)
  checkEquals(processedData[2,2],-1)
  checkEquals(processedData[3,2],1)
}
