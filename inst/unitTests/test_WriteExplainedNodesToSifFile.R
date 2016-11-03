# Test suite for the WriteExplainedNodesToSifFile function

test_WriteExplainedNodesToSifFileProducesTheCorrectSifFileWithDefaultNameAndCorrectOnly <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 2
    tempfile <- tempfile()
    outputDir <- paste(dirname(tempfile), as.integer(runif(1, 1, 1e9)), sep="-")


    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir, correctlyExplainedOnly = TRUE)
    
    # check number of files is correct
    checkEquals(length(list.files(outputDir, pattern = "*.sif")), 1)
    
    # Check data is correct
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-testNetwork1-testData1-delta2-Node0+.sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 5)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
    checkEquals(c(outputData[2,1], outputData[2,2], outputData[2,3]), c("Node0", "Inhibits", "Node2"))
    checkEquals(c(outputData[3,1], outputData[3,2], outputData[3,3]), c("Node1", "Activates", "Node3"))
    checkEquals(c(outputData[4,1], outputData[4,2], outputData[4,3]), c("Node2", "Activates", "Node6"))
    checkEquals(c(outputData[5,1], outputData[5,2], outputData[5,3]), c("Node2", "Activates", "Node7"))
}


test_WriteExplainedNodesToSifFileProducesTheCorrectSifFileWithCustomNameAndCorrectOnly <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 2
    tempfile <- tempfile()
    outputDir <- paste(dirname(tempfile), as.integer(runif(1, 1, 1e9)), sep="-")
    outputFilesName <- basename(tempfile)
    
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir, outputFilesName, correctlyExplainedOnly = TRUE)
    
    # check number of files is correct
    checkEquals(length(list.files(outputDir, pattern = "*.sif")), 1)
    
    # Check data is correct
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-", basename(tempfile), ".sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 5)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
    checkEquals(c(outputData[2,1], outputData[2,2], outputData[2,3]), c("Node0", "Inhibits", "Node2"))
    checkEquals(c(outputData[3,1], outputData[3,2], outputData[3,3]), c("Node1", "Activates", "Node3"))
    checkEquals(c(outputData[4,1], outputData[4,2], outputData[4,3]), c("Node2", "Activates", "Node6"))
    checkEquals(c(outputData[5,1], outputData[5,2], outputData[5,3]), c("Node2", "Activates", "Node7"))
}


test_WriteExplainedNodesToSifFileProducesTheCorrectSifFilesWithDefaultNames <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 2
    tempfile <- tempfile()
    outputDir <- paste(dirname(tempfile), as.integer(runif(1, 1, 1e9)), sep="-")
    
    
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir)
    
    # check number of files is correct
    checkEquals(length(list.files(outputDir, pattern = "*.sif")), 3)
    
    # Check data is correct
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-testNetwork1-testData1-delta2-Node0+.sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 5)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
    checkEquals(c(outputData[2,1], outputData[2,2], outputData[2,3]), c("Node0", "Inhibits", "Node2"))
    checkEquals(c(outputData[3,1], outputData[3,2], outputData[3,3]), c("Node1", "Activates", "Node3"))
    checkEquals(c(outputData[4,1], outputData[4,2], outputData[4,3]), c("Node2", "Activates", "Node6"))
    checkEquals(c(outputData[5,1], outputData[5,2], outputData[5,3]), c("Node2", "Activates", "Node7"))
    
    outputData <- read.table(paste0(outputDir, "/incorExplainedNodes-testNetwork1-testData1-delta2-Node0+.sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 1)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Inhibits", "Node2"))

    outputData <- read.table(paste0(outputDir, "/ambExplainedNodes-testNetwork1-testData1-delta2-Node0+.sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 4)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
    checkEquals(c(outputData[2,1], outputData[2,2], outputData[2,3]), c("Node0", "Inhibits", "Node2"))
    checkEquals(c(outputData[3,1], outputData[3,2], outputData[3,3]), c("Node1", "Inhibits", "Node5"))
    checkEquals(c(outputData[4,1], outputData[4,2], outputData[4,3]), c("Node2", "Inhibits", "Node5"))
}


test_WriteExplainedNodesToSifFileProducesTheCorrectAnnoFileWithDefaultNameAndCorrectOnly <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 2
    tempfile <- tempfile()
    outputDir <- paste(dirname(tempfile), as.integer(runif(1, 1, 1e9)), sep="-")
    
    
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir, correctlyExplainedOnly = TRUE)
    
    # check number of files is correct
    checkEquals(length(list.files(outputDir, pattern = "*_anno.txt")), 1)
    
    # Check data is correct
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-testNetwork1-testData1-delta2-Node0+_anno.txt"), stringsAsFactors = FALSE, header = TRUE)
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


test_WriteExplainedNodesToSifFileProducesTheCorrectAnnoFileWithCustomNameAndCorrectOnly <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 2
    tempfile <- tempfile()
    outputDir <- paste(dirname(tempfile), as.integer(runif(1, 1, 1e9)), sep="-")
    outputFilesName <- basename(tempfile)
    
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir, outputFilesName, correctlyExplainedOnly = TRUE)
    
    # check number of files is correct
    checkEquals(length(list.files(outputDir, pattern = "*_anno.txt")), 1)
    
    # Check data is correct
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-", basename(tempfile), "_anno.txt"), stringsAsFactors = FALSE, header = TRUE)
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


test_WriteExplainedNodesToSifFileProducesTheCorrectAnnoFilesWithDefaultNames <- function() {
    hypothesisnode <- "Node0"
    signOfHypothesis <- +1
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 2
    tempfile <- tempfile()
    outputDir <- paste(dirname(tempfile), as.integer(runif(1, 1, 1e9)), sep="-")
    
    
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir)
    
    # check number of files is correct
    checkEquals(length(list.files(outputDir, pattern = "*.sif")), 3)
    
    # Check data is correct
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-testNetwork1-testData1-delta2-Node0+_anno.txt"), stringsAsFactors = FALSE, header = TRUE)
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
    
    outputData <- read.table(paste0(outputDir, "/incorExplainedNodes-testNetwork1-testData1-delta2-Node0+_anno.txt"), stringsAsFactors = FALSE, header = TRUE)
    checkEquals(nrow(outputData), 1)
    checkEquals(names(outputData), c("NodeID", "Regulation"))
    checkEquals(outputData[1,1], "Node2")
    checkEquals(outputData[1,2], 1)

    outputData <- read.table(paste0(outputDir, "/ambExplainedNodes-testNetwork1-testData1-delta2-Node0+_anno.txt"), stringsAsFactors = FALSE, header = TRUE)
    checkEquals(nrow(outputData), 1)
    checkEquals(names(outputData), c("NodeID", "Regulation"))
    checkEquals(outputData[1,1], "Node5")
    checkEquals(outputData[1,2], -1)
}


test_WriteExplainedNodesToSifFileProducesTheCorrectOutputFileWithHighDelta  <- function() {
hypothesisnode <- "Node0"
signOfHypothesis <- +1
networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
network <- CausalR::CreateCCG(networkFile)
experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
delta <- 10
tempfile <- tempfile()
outputDir <- dirname(tempfile)
outputFilesName <- basename(tempfile)

CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir, outputFilesName, quiet=FALSE)

outputData <- read.table(paste0(outputDir, "/corExplainedNodes-", basename(tempfile), ".sif"), stringsAsFactors = FALSE)
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
    networkFile <- system.file(package='CausalR', 'extdata', 'testNetwork1.sif')
    network <- CausalR::CreateCCG(networkFile)
    experimentalDataFile <- system.file(package='CausalR', 'extdata', 'testData1.txt')
    experimentalData <- CausalR::ReadExperimentalData(experimentalDataFile, network)
    delta <- 1
    tempfile <- tempfile()
    outputDir <- dirname(tempfile)
    outputFilesName <- basename(tempfile)
    
    CausalR::WriteExplainedNodesToSifFile(hypothesisnode, signOfHypothesis, network, experimentalData,  delta, outputDir, outputFilesName, quiet=FALSE)
    
    outputData <- read.table(paste0(outputDir, "/corExplainedNodes-", basename(tempfile), ".sif"), stringsAsFactors = FALSE)
    checkEquals(nrow(outputData), 1)
    checkEquals(c(outputData[1,1], outputData[1,2], outputData[1,3]), c("Node0", "Activates", "Node1"))
}