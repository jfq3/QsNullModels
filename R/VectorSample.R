#' Randomize a binary vector
#' 
#' This function is used by Gotelli's matrix randomization functions. It takes an input 
#'   binary vector and a weight vector of equal length and reassigns 1s randomly in 
#'   proportion to the weights.
#'
#' @aliases VectorSample
#' @usage VectorSample(v, w)
#' @param v A binary vector
#' @param w A vector of sampling weights; length equal to length of v.
#' @return A randomized binary vector.
#' @export
#' @references Gotelli, N. J. 2000. Null model analysis of species co-occurrence patterns.
#'   Ecology 81:2606-2621.
#' @author Nicholas J. Gotelli. Help edited by John Quensen.
#' @examples
#' sam <- sample(c(1,0), 10, replace=TRUE)
#' wt <- rep(1, 10) # Equal sampling probablilities.
#' random.sam <- VectorSample(sam, wt)

VectorSample <- function(v,w) 
  
{
  x <- mat.or.vec(length(v),1)                   # creates a vector of 0s
  x[sample(1:length(v),size=sum(v),prob=w)] <- 1  # fills with 1s, sampling with weights
  return(x)
}
