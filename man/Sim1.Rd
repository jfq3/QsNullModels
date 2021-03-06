% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sim1.R
\name{Sim1}
\alias{Sim1}
\title{Gotelli's Sim1 Function}
\usage{
Sim1(m)
}
\arguments{
\item{m}{A binary community matrix.}
}
\value{
A randomized binary matrix of the same dimensions as the input matrix.
}
\description{
Randomizes a binary matrix m by reshuffling all of its elements equiprobably.
}
\examples{
m <- sample(c(1,0), 50, replace=TRUE)
m <- matrix(m, 5, 10)
random.m <- Sim1(m)
}
\references{
Gotelli, N. J. 2000. Null model analysis of species co-occurrence
  patterns. Ecology 81:2606-2621.
}
\author{
Nicholas J. Gotelli. Help edited by John Quensen.
}
