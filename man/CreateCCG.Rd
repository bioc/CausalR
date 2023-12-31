% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateCCG.r
\name{CreateCCG}
\alias{CreateCCG}
\title{create a Computational Causal Graph (CCG)}
\usage{
CreateCCG(filename, nodeInclusionFile = NULL, excludeNodesInFile = TRUE)
}
\arguments{
\item{filename}{file name of the network file (in .sif file format)}

\item{nodeInclusionFile}{optional path to a text file listing nodes to exclude in the CCG (or include - see argument excludeNodesInFile).}

\item{excludeNodesInFile}{flag to determine if nodes in inclusion file should be taken as nodes to include or nodes to exclude. Default is TRUE to exclude.}
}
\value{
an igraph object containing the CCG.
}
\description{
Creates a computational causal graph from a network file.
}
\note{
CreateCG and CreateCCG create causal and computational causal graphs respectively.
}
\examples{
# get path to example .sif file
network <- system.file(package='CausalR', 'extdata', 'testNetwork.sif')
#create ccg
ccg = CreateCCG(network)


}
\references{
L Chindelevitch et al.
Causal reasoning on biological networks: Interpreting transcriptional changes.
Bioinformatics, 28(8):1114-21, 2012.
}
\concept{
CausalR
}

