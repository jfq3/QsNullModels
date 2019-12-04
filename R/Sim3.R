#' Gotelli's Sim3 Function
#' 
#' Randomizes a binary matrix m by reshuffling elements within each column equiprobably.
#' 
#' @usage Sim3(m)
#' @aliases Sim3
#' @param m A binary community matrix.
#' @return A binary matrix randomized by columns and of the same dimensions as the input matrix.
#' @export
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns. Ecology 81:2606-2621.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.m <- Sim3(m)
Sim3 <- function(m) 
{
  apply(m,2,sample)
}
