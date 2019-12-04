#' Sim8 Function
#' 
#' Randomizes a binary matrix m by reshuffling all elements. Columns are proportional
#'   to column sums, rows proportional to row sums.
#' 
#' @aliases Sim8
#' @usage Sim8(m)
#' @param m A binary community matrix.
#' @return A randomized binary matrix of the same dimensions as the input matrix.
#' @details Makes a call to VectorSample.
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @export
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.sam <- Sim8(m)
Sim8 <- function(m) 
{
  Matrix.Weights <- outer(rowSums(m),colSums(m))
  matrix(VectorSample(m,w=Matrix.Weights),ncol=ncol(m))
}
