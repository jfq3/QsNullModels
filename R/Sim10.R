#' Gotelli's Sim10 Function
#' 
#' Randomizes a binary matrix m by reshuffling all elements. Rows & columns proportional
#'   to supplied row and column weights.
#' 
#' @aliases Sim10
#' @usage Sim10(m, Row.Weights, Col.Weights)
#' @param m A binary community matrix.
#' @param Row.Weights User supplied sampling weights for rows.
#' @param Col.Weights User supplied sampling weights for columns.
#' @return A randomized binary matrix of the same dimensions as the input matrix.
#' @export
#' @details Makes a call to the function VectorSample.
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' data(com)
#' rw <- sample(1:10, nrow(com), replace=TRUE)
#' cw <- sample(1:10, ncol(com), replace=TRUE)
#' rand.com <- Sim10(com, Row.Weights=rw, Col.Weights=cw)
Sim10 <- function(m,Row.Weights,Col.Weights) 
{
  Matrix.Weights <- outer(Row.Weights,Col.Weights)
  matrix(VectorSample(m, w=Matrix.Weights),ncol=ncol(m))
}

