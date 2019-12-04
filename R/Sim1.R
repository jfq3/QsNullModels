#' Gotelli's Sim1 Function
#' 
#' Randomizes a binary matrix m by reshuffling all of its elements equiprobably.
#' @aliases Sim1
#' @usage Sim1(m)
#' 
#' @param m A binary community matrix.
#'
#' @return A randomized binary matrix of the same dimensions as the input matrix.
#' @export
#' 
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence
#'   patterns. Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.m <- Sim1(m)
Sim1 <- function(m) 
{
  matrix(sample(m), ncol=ncol(m))
}

