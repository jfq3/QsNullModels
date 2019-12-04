#' Gotelli's Sim4 Function
#' 
#' Randomizes a binary matrix m by shuffling elements within each row. Sampling weights
#'   for each column are proportional to column sums.
#'   
#' @aliases Sim4
#' @usage Sim4(m)
#' @param m A binary community matrix.
#' @details Makes a call to VectorSample.
#' @return A binary matrix randomized by columns and of the same dimensions as the
#'   input matrix.
#' @export
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence 
#'   patterns. Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.m <- Sim4(m)
Sim4 <- function(m) 
  
{
  t(apply(m,1,VectorSample,w=colSums(m)))
}
