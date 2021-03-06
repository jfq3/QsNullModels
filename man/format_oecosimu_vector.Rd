% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_oecosimu_vector.R
\name{format_oecosimu_vector}
\alias{format_oecosimu_vector}
\title{Reformat an oecosimu Vector}
\usage{
format_oecosimu_vector(sol, com)
}
\arguments{
\item{sol}{Result of oecosimu when statistic is a vector.}

\item{com}{Community matrix used to calculate oecosimu result.}
}
\value{
A data frame.
}
\description{
Reformat an oecosimu Vector
}
\details{
Vegan's oecosimu function is used to compare an observed and expected value or values based on a null model. A drawback of the oecosimu summary output is that sample pairs are not named. This function reformats the oecosimu result into a table giving the names of sample pairs, the observed statistic, the expected statistic and its standard deviation based on the null model, the standard effect size, and th probability that the observed and expected values of the statistic are not different. This function is appropriate when the statistic is a vector rather than a distance matrix.
}
\seealso{
format_oecosimu_dist
}
\author{
John Quensen
}
