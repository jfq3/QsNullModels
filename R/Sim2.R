#' Gotelli's Sim2 Function
#' 
#' Randomizes a binary matrix m by reshuffling elements within each row equiprobably.
#'
#' @usage Sim2(m)
#' @param m A binary community matrix.
#' @return A binary matrix randomized by rows of the same dimensions as the input matrix.
#' @export
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns. Ecology 81:2606-2621.
#' @examples
#' m <- sample(c(1,0), 50, replace=TRUE)
#' m <- matrix(m, 5, 10)
#' random.m <- Sim2(m)
Sim2 <- function(m) 
  
{
  t(apply(m,1,sample))
}
