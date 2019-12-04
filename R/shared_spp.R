#' Shared Species
#' 
#' Calculates shared species between all samples in a presenece/absence community matrix.
#' 
#' @aliases shared_spp
#' @usage shared_spp(com)
#'
#' @param com A community matrix with samples as rows and species as columns.
#'
#' @return A vector of the number of shared species..
#' @export
#' 
#' @details If presented with a community matrix of count data, this function first 
#'   converts it to presence/absence.
#' @author John Quensen
#' 
#' @examples
#' data(com)
#' shared_spp(com)
#' @importFrom vegan decostand
#' 
shared_spp <- function(com) {
  x <- as.matrix(com)
  x <- decostand(x, "pa")
  N <- nrow(x)
  tri <- matrix(FALSE, N, N)
  tri <- row(tri) > col(tri)
  ss <-  tcrossprod(x)[tri]
  return(ss)
}