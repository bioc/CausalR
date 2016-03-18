# Test suite for the WriteExplainedNodesToSifFile function

test_WriteExplainedNodesToSifFileProducesTheCorrectOutputFile <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    network <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    ccg <- CausalR::CreateCCG(network)
    data <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    delta <- 2
    
    tempfile <- tempfile()
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, ccg, data,  delta, file=tempfile, display=TRUE)
    
    outputData <- read.table(paste0(tempfile, ".sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 5)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
    checkEquals(c(outputData[2,1], outputData[2,2], outputData[2,3]), c("Node0", "Inhibits", "Node2"))
    checkEquals(c(outputData[3,1], outputData[3,2], outputData[3,3]), c("Node1", "Activates", "Node3"))
    checkEquals(c(outputData[4,1], outputData[4,2], outputData[4,3]), c("Node2", "Activates", "Node6"))
    checkEquals(c(outputData[5,1], outputData[5,2], outputData[5,3]), c("Node2", "Activates", "Node7"))
}

test_WriteExplainedNodesToSifFileProducesTheCorrectOutputFileWithHighDelta  <- function() {
hypothesisnode <- "Node0"
signOfHypothesis <- +1
network <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
ccg <- CausalR::CreateCCG(network)
data <- system.file(package='CausalR', 'extdata', 'testData1.txt')
delta <- 10

tempfile <- tempfile()
CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, ccg, data,  delta, file=tempfile, display=TRUE)

outputData <- read.table(paste0(tempfile, ".sif"), stringsAsFactors = FALSE)
checkEquals(nrow(outputData), 5)
checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
checkEquals(c(outputData[2,1], outputData[2,2], outputData[2,3]), c("Node0", "Inhibits", "Node2"))
checkEquals(c(outputData[3,1], outputData[3,2], outputData[3,3]), c("Node1", "Activates", "Node3"))
checkEquals(c(outputData[4,1], outputData[4,2], outputData[4,3]), c("Node2", "Activates", "Node6"))
checkEquals(c(outputData[5,1], outputData[5,2], outputData[5,3]), c("Node2", "Activates", "Node7"))
}

test_WriteExplainedNodesToSifFileProducesTheCorrectOutputFileWithDeltaOfOne  <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    network <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    ccg <- CausalR::CreateCCG(network)
    data <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    delta <- 1
    
    tempfile <- tempfile()
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, ccg, data,  delta, file=tempfile, display=TRUE)
    
    outputData <- read.table(paste0(tempfile, ".sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 1)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
}

test_WriteExplainedNodesToSifFileProducesTheCorrectAnnotationFile <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    network <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    ccg <- CausalR::CreateCCG(network)
    data <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    delta <- 2
    
    tempfile <- tempfile()
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, ccg, data,  delta, file=tempfile, display=TRUE)
    
    outputData <- read.table(paste0(tempfile, "_anno.txt"), stringsAsFactors = FALSE, header = TRUE)
    
    checkEquals(nrow(outputData), 5)
    checkEquals(names(outputData), c("NodeID", "Regulation"))
    checkEquals(outputData[1,1], "Node0")
    checkEquals(outputData[1,2], 1)
    checkEquals(outputData[2,1], "Node1")
    checkEquals(outputData[2,2], 1)
    checkEquals(outputData[3,1], "Node3")
    checkEquals(outputData[3,2], 1)
    checkEquals(outputData[4,1], "Node6")
    checkEquals(outputData[4,2], -1)
    checkEquals(outputData[5,1], "Node7")
    checkEquals(outputData[5,2], -1)
}
