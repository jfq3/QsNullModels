#' Gotelli's Sim5 Function
#' 
#' Randomizes a matrix m by reshuffling elements within each column. Sampling weights
#'   for each row are proportional to row sums.
#' 
#' @aliases Sim5
#' @usage Sim5(m)
#' @param m A binary community matrix.
#' @return A binary matrix randomized by columns of the same dimensions as the input matrix.
#' @export
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.m <- Sim5(m)
Sim5 <- function(m) 
{
  apply(m,2,VectorSample,w=rowSums(m))
}
