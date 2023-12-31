% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetScoresForSingleNode.r
\name{GetScoresForSingleNode}
\alias{GetScoresForSingleNode}
\title{Get scores for single node}
\usage{
GetScoresForSingleNode(iNode, timeToRunSoFar, nodesToBeTested, network, delta,
  processedExperimentalData, numPredictions, epsilon, useCubicAlgorithm,
  use1bAlgorithm, symmetricCCG, correctPredictionsThreshold,
  experimentalDataStats, quiet)
}
\arguments{
\item{iNode}{this node}

\item{timeToRunSoFar}{the time to run so far}

\item{nodesToBeTested}{List of all nodes to be tested}

\item{network}{Computational Causal Graph, as an igraph.}

\item{delta}{Distance to search within the causal graph.}

\item{processedExperimentalData}{The processed experimental data}

\item{numPredictions}{The number of predictions}

\item{epsilon}{The threshold that is used when calculating the p-value using the cubic algorithm (see 'Assessing statistical significance in causal graphs').}

\item{useCubicAlgorithm}{An indicator specifying which algorithm will be used to calculate the p-value. The default is set as useCubicAlgorithm = TRUE which uses the cubic algorithm. If this value is set as FALSE, the algorithm will use the much slower quartic algorithm which does compute the exact answer, as opposed to using approximations like the cubic algorithm.}

\item{use1bAlgorithm}{An indicator specifying whether the 1a or 1b (default, faster) variant of the cubic algorithm described in Chindelevitch's paper will be used to calculate the p-value.}

\item{symmetricCCG}{This flag specifies whether the CCG is assumed to be symmetric. The value is set as TRUE as a default. If this is the case the running time of the algorithm is reduced since the negative node values can be calculated using symmetry and the results of calculations performed for the positive node}

\item{correctPredictionsThreshold}{A threshold on the number of correct predictions for a given hypothesis. If a hypothesis produces fewer correct predictions than predictionsThreshold then the algorithm will not calculate the two p-values. Instead 'NA' will be displayed in the final two columns of the corresponding row of the results table. As a default correctPredictionsThreshold is set as -Inf, so that the p-values are calculated for all specified hypotheses. Note: Set to Inf to turn off p-value calculations entirely.}

\item{experimentalDataStats}{Stats from the experimental data}

\item{quiet}{a flag to supress progress output}
}
\value{
If symmetricCCG is false, this returns a single line of the scoreMatrix for the 'iNode'th node in nodesToBeTested. If symmetricCCG is true this returns two lines. The first of which corresponds to the positive node and the second the negative node.
}
\description{
A helper function for RankTheHypotheses to calculate a line of the scoresMatrix table
}

