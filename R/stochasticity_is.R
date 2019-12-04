#' Stochasticity by Independent Swap
#' 
#' This function calculates stochasticity based on the Jaccard dissimilarity as defined
#'   in Zhou et al. (2014) by randomizing a binary community matrix with picante's 
#'   independentswap algorithm.
#' 
#' @aliases stochasticity_is
#' @usage stochasticity_is(com, n.reps=999)
#' @param com A binary community matrix.
#' @param n.reps Number of replications.
#' @return A vector of stochasticity values.
#' @export
#' @details The binary community matrix is randomized using picante's function 
#'   randomizeMatrix with method="independentswap." The independent swap algorithm 
#'   maintains both row and column totals.
#' @references Zhou, J. Z., Y. Deng, P. Zhang, K. Xue, Y. T. Liang, J. D. Van Nostrand,
#'   Y. F. Yang, Z. L. He, L. Y. Wu, D. A. Stahl, T. C. Hazen, J. M. Tiedje, and 
#'   A. P. Arkin. 2014. Stochasticity, succession, and environmental perturbations 
#'   in a fluidic ecosystem. Proceedings of the National Academy of Sciences of the 
#'   United States of America 111:E836-E845.
#' @author John Quensen
#' @importFrom vegan vegdist
#' @importFrom picante randomizeMatrix
stochasticity_is <- function(com, n.reps=999) {
  f <- function(x) {
    as.matrix(1-(vegdist(randomizeMatrix(x, "independentswap", iterations=n.reps), "jac", binary=TRUE)))
  }
  nulls <- replicate(n.reps, f(com))
  # Calculate mean Jexp's from the randomizations.
  nulls.means <- apply(nulls, c(1:2), mean, na.rm=TRUE)
  # Calculate Jobs's.
  obs <- as.matrix(1-(vegdist(com, "jac", binary=TRUE)))
  # Calculat SS's as defined by Zhou et al., 2014.
  ss <- matrix2list((obs-nulls.means)/obs)
  # Calcuate % stochasticity.
  stoch <- 1-ss
  return(stoch)
}
