#' Gotelli's Sim7 Function
#' 
#' Randomizes a binary matrix m by reshuffling all elements. Columns are equiprobable,
#'   rows proportional to row sums.
#' 
#' @aliases Sim7
#' @usage Sim7(m)
#' @param m A binary matrix.
#' @return A randomized binary matrix.
#' @export
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.sam <- Sim7(m)
#' 
Sim7 <- function(m) 
{
  Matrix.Weights <- outer(rowSums(m),rep(1,ncol(m)))
  matrix(VectorSample(m, w=Matrix.Weights),ncol=ncol(m))
}
