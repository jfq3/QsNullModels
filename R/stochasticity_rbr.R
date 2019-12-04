#' Stochasticity by randomize_by_rows
#' 
#' This function calculates stochasticity based on the Jaccard dissimilarity as defined
#'   in Zhou et al. (2014) by randomizing a binary community matrix using the function 
#'   randomize_by_rows in this package.
#' 
#' @aliases stochasticity_rbr
#' @usage stochasticity_rbr(com, w, n.reps=999)
#' @param com A binary community matrix.
#' @param w A vector of sampling wieghts.
#' @param n.reps Number of replications.
#' @return A vector of stochasticity values.
#' @export
#' @details The binary community matrix is randomized using the function randomize_by_rows
#'   in this package. This method maintains row (sample) totals.
#'   
#'   The user must supply w, a vector of sampling weights. If w equals the column sums of
#'   com, the method corresponds to Ye Deng's gamma.method=sample (see reference). If w 
#'   equals the column sums of a larger experiment matrix of which com is a sub-set, the
#'   method corresponds to gamma.method=total.
#' @references Zhou, J. Z., Y. Deng, P. Zhang, K. Xue, Y. T. Liang, J. D. Van Nostrand, 
#'   Y. F. Yang, Z. L. He, L. Y. Wu, D. A. Stahl, T. C. Hazen, J. M. Tiedje, and 
#'   A. P. Arkin. 2014. Stochasticity, succession, and environmental perturbations in 
#'   a fluidic ecosystem. Proceedings of the National Academy of Sciences of the United
#'   States of America 111:E836-E845.
#' @author Johon Quensen
#' @importFrom vegan vegdist
stochasticity_rbr <- function(com, w, n.reps=999) {
  f <- function(x, w) {
    as.matrix(1-(vegdist(randomize_by_rows(com, w), "jac", binary=TRUE)))
  }
  nulls <- replicate(n.reps, f(com, w))
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
