% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BiRankFr.R
\name{BiRankFr}
\alias{BiRankFr}
\title{BiRank algorithm}
\usage{
BiRankFr(
  sNetwork,
  fraudMat,
  Today = Sys.Date(),
  decayR = 0,
  decayF = 0,
  alpha = 0.85,
  maxiter = 1000,
  Epsilon = 1e-14,
  PrintProgress = F,
  pInit = NULL,
  cInit = NULL,
  ConvCriterion = c("Sep", "Whole", "Order")
)
}
\arguments{
\item{sNetwork}{Data frame containing the variables startNode, endNode and possibly Date.}

\item{fraudMat}{Data frame containing the variables FraudInd (binary variable indicatinf fraudulent claimes) and possibly Date.}

\item{Today}{Date of analysis (e.g. 1/1/2020), default is \code{Sys.Date()}. Supply either as object with class \code{Date}
or as character string in the format \%d/\%m/\%Y.}

\item{decayR}{Parameter for exponential decay of recency of relation (in weeks).}

\item{decayF}{Parameter for exponential decay of recency of fraud (in weeks).}

\item{alpha}{Damping factor for propagation algorithm (return to start).}

\item{maxiter}{Maximum number of iterations for propagation algorithm.}

\item{Epsilon}{Positive convergence tolerance \eqn{\epsilon}.}

\item{PrintProgress}{Logical, indicates whether progress of the algorithm has to be printed.}

\item{pInit}{Initial values for the party score vector \bold{\eqn{p}}.}

\item{cInit}{Initial values for the fraud score vector \bold{\eqn{c}}.}

\item{ConvCriterion}{Which convergence criterion to use. \code{"Sep"} uses
\eqn{||p - p_{old}||_{2} / ||p_{old}||_{2} < \epsilon} and \eqn{||c - c_{old}||_{2} / ||c_{old}||_{2} < \epsilon}.
\code{"Whole"} uses \eqn{||x - x_{old}||_{2} / ||x_{old}||_{2} < \epsilon} with \code{x = c(c, p)}. \code{"Order"} uses
the same convergence criterion as \code{"Sep"} and checks if the order of the elements of \eqn{\bold{p}} and \eqn{\bold{c}}
has not changed since the previous iteration.}
}
\value{
A list with the following components:

\item{ResultsClaims}{A data frame containing the claim IDs, fraud scores, scaled and normalized fraud scores.}

\item{ResultsParties}{A data frame containing the party IDs, party scores, scaled and normalized party scores.}

\item{AdjacencyMatrix}{Adjancency/weight matrix indicating which nodes are connected.}

\item{iter}{Number of iterations that the algorithm needed to converge.}

\item{Converged}{Logical, indicating whether the algorithm converged.}
}
\description{
BiRank algorithm to compute fraud scores.
}
\examples{
library(BiRankFraud)
NetwLabel = data.frame(
startNode = c('P2', 'P3', 'P3', 'C1', 'C1', 'C5', 'P1', 'P4', 'C2', 'C2'),
endNode = c('C3', 'C3', 'C4', 'P2', 'P3', 'P3', 'C1', 'C5', 'P1', 'P4'),
stringsAsFactors = FALSE
)
NetwLabel[grepl("C", NetwLabel$startNode), 1:2] = NetwLabel[grepl("C", NetwLabel$startNode), 2:1]
NetwLabel = NetwLabel[order(NetwLabel$startNode), ]
NetwLabel$startNode = as.numeric(gsub("P", "", NetwLabel$startNode))
NetwLabel$endNode   = as.numeric(gsub("C", "", NetwLabel$endNode))
NetwLabel

c0      = c(rep(0, 3), 1, 0)
Results = BiRankFr(NetwLabel, data.frame(FraudInd = c0))
Results
}
