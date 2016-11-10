## ----style-knitr,eval=TRUE,echo=FALSE,results="asis"---------------------

## ------------------------------------------------------------------------
library(CausalR)

## ----eval=FALSE----------------------------------------------------------
#  library(igraph)

## ------------------------------------------------------------------------
cg <- CreateCG(system.file( "extdata", "testNetwork1.sif", 
    package="CausalR"))

## ------------------------------------------------------------------------
PlotGraphWithNodeNames(cg)  # producing the following graph.

## ------------------------------------------------------------------------
ccg <- CreateCCG(system.file( "extdata", "testNetwork1.sif", 
    package="CausalR"))

## ------------------------------------------------------------------------
PlotGraphWithNodeNames(ccg) # producing the following graph.

## ------------------------------------------------------------------------
experimentalData <- ReadExperimentalData(system.file( "extdata", 
                        "testData1.txt", package="CausalR"),ccg)

## ---------------------------------------------------------------------------------------------------------------------
options(width=120)
RankTheHypotheses(ccg, experimentalData, delta=2) 

## ---------------------------------------------------------------------------------------------------------------------
options(width=120)
testlist<-c('Node0','Node2','Node3')
RankTheHypotheses(ccg, experimentalData, 
                                delta=2, listOfNodes=testlist)

## ---------------------------------------------------------------------------------------------------------------------
options(width=120)
RankTheHypotheses(ccg, experimentalData, 2, listOfNodes='Node0')

## ----results='hide'---------------------------------------------------------------------------------------------------
GetShortestPathsFromCCG(ccg, 'Node0', 'Node3')

## ---------------------------------------------------------------------------------------------------------------------
predictions <- MakePredictionsFromCCG('Node0',+1,ccg,2)
predictions

## ---------------------------------------------------------------------------------------------------------------------
ScoreHypothesis(predictions, experimentalData)

## ---------------------------------------------------------------------------------------------------------------------
GetNodeName(ccg,CompareHypothesis(predictions, experimentalData))

## ---------------------------------------------------------------------------------------------------------------------
options(width=120)
Rankfor4<-RankTheHypotheses(ccg, experimentalData, 2, 
                            correctPredictionsThreshold=4)
Rankfor4   # For example output only
subset(Rankfor4,Correct>=4)

## ---------------------------------------------------------------------------------------------------------------------
runSCANR(ccg, experimentalData, numberOfDeltaToScan=2, topNumGenes=4, 
         correctPredictionsThreshold=1,writeResultFiles = TRUE, 
         writeNetworkFiles = "none",quiet=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  AllData<-read.table(file="testData1.txt", sep = "\t")
#  DifferentialData<-AllData[AllData[,2]!=0,]
#  write.table(DifferentialData, file="DifferentialData.txt",
#      sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
#  
#  runSCANR(ccg, ReadExperimentalData("DifferentialData.txt", ccg),
#          NumberOfDeltaToScan=2,topNumGenes=100,
#          correctPredictionsThreshold=2)

## ----results='hide'---------------------------------------------------------------------------------------------------
testlist<-c('Node0','Node3','Node2')
RankTheHypotheses(ccg, experimentalData,2,listOfNodes=testlist)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  WriteExplainedNodesToSifFile("Node1", +1,ccg,experimentalData,delta=2)

## ---------------------------------------------------------------------------------------------------------------------
scanResults <- runSCANR(ccg, experimentalData, numberOfDeltaToScan=2, 
  topNumGenes=4,correctPredictionsThreshold=1,
  writeResultFiles = FALSE, writeNetworkFiles = "none",quiet=FALSE)
WriteAllExplainedNodesToSifFile(scanResults, ccg, experimentalData, 
  delta=2, correctlyExplainedOnly = TRUE, quiet = TRUE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  runSCANR(ccg, experimentalData, numberOfDeltaToScan=2,
#    topNumGenes=4,correctPredictionsThreshold=1,quiet=TRUE,
#    writeResultFiles = TRUE, writeNetworkFiles = "correct")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  CreateCCG(filename, nodeInclusionFile = 'NodesList.txt',
#                                      excludeNodesInFile = TRUE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Set-up
#  library(CausalR)
#  library(igraph)
#  
#  # Load network, create CG and plot
#  cg <- CreateCG('testNetwork1.sif')
#  
#  PlotGraphWithNodeNames(cg)

## ----results='hide'---------------------------------------------------------------------------------------------------
# Load network, create CCG and plot
ccg <- CreateCCG(system.file( "extdata", "testNetwork1.sif", 
    package="CausalR"))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  PlotGraphWithNodeNames(ccg)

## ----results='hide'---------------------------------------------------------------------------------------------------
# Load experimental data
experimentalData <- ReadExperimentalData(system.file( "extdata",
                        "testData1.txt", package="CausalR"),ccg)

## ----results='hide'---------------------------------------------------------------------------------------------------
# Make predictions for all hypotheses, with pathlength set to 2.
RankTheHypotheses(ccg, experimentalData, 2)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Make predictions for all hypotheses, running in parallel
#  # NOTE: this requires further set-up as detailed in Appendix B.
#  RankTheHypotheses(ccg,experimentalData,delta,doParallel=TRUE)

## ----results='hide'---------------------------------------------------------------------------------------------------
# Make predictions for a single node (results for + and - 
# hypotheses for the node will be generated),
RankTheHypotheses(ccg, experimentalData,2,listOfNodes='Node0')

## ----results='hide'---------------------------------------------------------------------------------------------------
# Make predictions for an arbitrary list of nodes (gives results
# for up- and down-regulated hypotheses for each named node),
testlist <- c('Node0','Node3','Node2')
RankTheHypotheses(ccg, experimentalData,2,listOfNodes=testlist)

## ----results='hide'---------------------------------------------------------------------------------------------------
# An example of making predictions for a particular signed hypo-
# -thesis at delta=2, for up-regulated node0, i.e.node0+.
# (shown to help understanding of hidden functionality)
predictions<-MakePredictionsFromCCG('Node0',+1,ccg,2)
GetNodeName(ccg,CompareHypothesis(predictions,experimentalData))

## ----results='hide'---------------------------------------------------------------------------------------------------
# Scoring the hypothesis predictions
ScoreHypothesis(predictions,experimentalData)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Compute statistics required for Calculating Significance
#  # p-value
#  Score<-ScoreHypothesis(predictions,experimentalData)
#  CalculateSignificance(Score, predictions, experimentalData)
#  PreexperimentalDataStats <-
#      GetNumberOfPositiveAndNegativeEntries(experimentalData)
#  
#      #this gives integer values for n_+ and n_- for the
#      #experimental data,as shown in Table 2.
#  
#      PreexperimentalDataStats
#  
#      # add required value for n_0, number of non-differential
#      # experimental results,
#      experimentalDataStats<-c(PreexperimentalDataStats,1)
#      # then use,
#      AnalysePredictionsList(predictions,8)
#      # ...to output integer values q_+, q_- and q_0 for
#      #    significance calculations (see Table 2)
#      # then store this in the workspace for later use,
#      predictionListStats<-AnalysePredictionsList(predictions,8)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Compute Significance p-value using default cubic algorithm
#  CalculateSignificance(Score,predictionListStats,
#      experimentalDataStats, useCubicAlgorithm=TRUE)
#  # or simply,
#  CalculateSignificance(Score,predictionListStats,
#      experimentalDataStats)
#  # as use cubic algorithm is the default setting.

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Compute Significance p-value using default quartic algorithm
#  CalculateSignificance(Score,predictionListStats,
#                  experimentalDataStats,useCubicAlgorithm=FALSE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Compute enrichment p-value
#  CalculateEnrichmentPvalue(predictions, experimentalData)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Running SCAN whilst excluding scoring of hypotheses for non-
#  # -differential nodes
#  AllData<-read.table(file="testData1.txt", sep="\t")
#  DifferentialData<-AllData[AllData[,2]!=0,]
#  write.table(DifferentialData, file="DifferentialData.txt",
#      sep="\t",row.names=FALSE, col.names=FALSE, quote=FALSE )
#  
#  runSCANR(ccg, ReadExperimentalData("DifferentialData.txt", ccg),
#          NumberOfDeltaToScan=3, topNumGenes=100,
#          correctPredictionsThreshold=3)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Indirect Individual Hypothesis Network Generation (after running SCAN)
#  WriteExplainedNodesToSifFile("Node1", +1,ccg,experimentalData,delta=2)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Indirect Network Generation for All Hypotheses (after running SCAN)
#  scanResults <- runSCANR(ccg, experimentalData, numberOfDeltaToScan=2,
#    topNumGenes=4,correctPredictionsThreshold=1,
#    writeResultFiles = FALSE, writeNetworkFiles = "none",quiet=FALSE)
#  WriteAllExplainedNodesToSifFile(scanResults, ccg, experimentalData,
#    delta=2, correctlyExplainedOnly = TRUE, quiet = TRUE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # Direct Network Generation for All Hypotheses (whilst running SCAN)
#  runSCANR(ccg, experimentalData, numberOfDeltaToScan=2,
#    topNumGenes=4,correctPredictionsThreshold=1,quiet=TRUE,
#    writeResultFiles = TRUE, writeNetworkFiles = "correct")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  RankTheHypotheses(ccg,experimentalData,delta,doParallel=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  RankTheHypotheses(ccg,experimentalData,delta,
#                                    doParallel=TRUE, numCores=3)

## ---------------------------------------------------------------------------------------------------------------------
library(compiler)
enableJIT=3

