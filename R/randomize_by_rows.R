#' Randomize by Rows
#' 
#' This function randomizes a binary matrix by shuffling the values in rows using sampling 
#'   probabilities specified in vector w. It is similar to Sim4 but has the advantage that 
#'   the user may specify w instead of having it calculated from the column sums.
#' 
#' @aliases randomize_by_rows
#' 
#' @usage randomize_by_rows(com, w, rename=FALSE)
#'
#' @param com A binary community matrix with samples as rows and taxaa as columns.
#' @param w Sampling weights in a vector of length equal to row length in com.
#' @param rename A logical indicating whether or not samples should be renamed.
#' 
#' @details The number of non-zero entries in w must be at least the maximum number of 
#'   non-zero entries in a row of com (max(rowSums(com)).
#' @return A randomized binary matrix.
#' @export
#' @author John Quensen
#' 
randomize_by_rows <- function(com, w, rename=FALSE) {
  x <-t(apply(com,1,VectorSample,w))
  colnames(x) <- colnames(com)
  if (rename==TRUE) {rownames(x) <- paste("R", rownames(com), sep="_")}
  else {rownames(x) <- rownames(com)}
  return(x)
}
