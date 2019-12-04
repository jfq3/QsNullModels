#' Convert a square matrix to a list
#' 
#' This function converts a square matrix as returned by a null model calculation for a similarity or distance into a list.
#'
#' @usage matrix2list(x)
#' @param x A square matrix.
#' @return A vector.
#' @author John Quensen
#' @importFrom stats as.dist
matrix2list <- function(x) {
  a <- as.dist(x, upper=FALSE, diag=FALSE)
  attributes(a) <- NULL
  return(a)
}
