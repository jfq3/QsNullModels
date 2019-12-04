#' Gotelli's Sim6 Function
#' 
#' Randomizes a binary matrix m by reshuffling all elements. Rows are equiprobable, 
#'   columns proportional to column sums.
#' 
#' @aliases Sim6
#' @usage Sim6(m)
#' @param m A binary community matrix.
#' @return A randomized binary matrix of the same dimensions as the input matrix.
#' @export
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.sam <- Sim6(m) 
Sim6 <- function(m) 
{
  Matrix.Weights <- outer(rep(1,nrow(m)),colSums(m))
  matrix(VectorSample(m, w=Matrix.Weights),ncol=ncol(m))
}
