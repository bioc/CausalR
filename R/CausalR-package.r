## Package level imports/exports
#' @import igraph
NULL

## Add package documentation
#' The CausalR package
#' 
#' Causal Reasoning algorithms for biological networks, including predictions, scoring, p-value calculation and ranking.
#' 
#' @rdname CausalR-package
#' @name CausalR-package
#' @aliases CausalR-package CausalR
#' @concept CausalR
#' @docType package
#' 
#' @details The most important functions are:
#' \itemize{
#'     \item{\code{\link{CreateCCG}}: read a computational causal graph from a .sif file}
#'     \item{\code{\link{ReadExperimentalData}}: read a experimental data from a .txt file}
#'     \item{\code{\link{MakePredictions}}: make causal reasoning predictions from a CCG}
#'     \item{\code{\link{ScoreHypothesis}}: score causal reasoning predictions}
#'     \item{\code{\link{CalculateSignificance}}: calculate statisitical significance of a result}
#'     \item{\code{\link{RankTheHypotheses}}: compare different possible hypotheses on a single CCG}
#' }
#' 
#' @references
#' \itemize{
#'     \item{"Causal reasoning on biological networks: interpreting transcriptional changes", Chindelevitch \emph{et al.}, Bioinformatics \bold{28} 1114 (2012). doi:\href{http://dx.doi.org/10.1093/bioinformatics/bts090}{10.1093/bioinformatics/bts090}}
#'     \item{"Assessing statistical significance in causal graphs", Chindelevitch \emph{et al.}, BMC Bioinformatics \bold{13} 35 (2012). doi:\href{http://dx.doi.org/10.1186/1471-2105-13-35}{10.1186/1471-2105-13-35}}
#' }
#' 
#' @author{
#'     David Riley \email{david.riley@tessella.com}, Chirag Mistry, Mark Pipe
#' }
NULL