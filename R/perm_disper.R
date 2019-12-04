#' Permutational Dispersion Analysis
#' 
#' This function tests if the dispersion about a group centroid is significantly different
#'   from null model expectation. It makes the test using Jaccard distances and randomizes 
#'   the matrix using the randomize_by_row function. It is based on the procedure used in  
#'   Zhou et al. (2014).
#' 
#' @aliases perm_disper
#' @usage perm_disper(com, w, n.reps=999)
#'
#' @param com A binary community matrix with samples in rows and taxa in columns.
#' @param w Sampling weights.
#' @param n.reps Number of replications.
#'
#' @return Result of vegan's betadisper test between observed and null model expectation.
#' @export
#' 
#' @details The binary community matrix is randomized using the function randomize_by_rows. 
#'   Sampling weights are species sums by default, but may be specified differently by the 
#'   user. This method maintains sample sums (species richness) and constrains species 
#'   occurrences.
#' 
#' @author John Quensen
#' 
#' @references Zhou, J. Z., Y. Deng, P. Zhang, K. Xue, Y. T. Liang, J. D. Van Nostrand, 
#'   Y. F. Yang, Z. L. He, L. Y. Wu, D. A. Stahl, T. C. Hazen, J. M. Tiedje, and A. P. 
#'   Arkin. 2014. Stochasticity, succession, and environmental perturbations in a fluidic 
#'   ecosystem. Proceedings of the National Academy of Sciences of the United States of 
#'   America 111:E836-E845.
#'  @importFrom vegan vegdist, betadisper
perm_disper <- function(com, w, n.reps=999)
{
  # Define function to replicate.
  jac_obs_null_chase <- function(com, w) {
    x <- randomize_by_rows(com, w)
    colnames(x) <- colnames(com)
    rownames(x) <- paste("R", rownames(com), sep="_")
    y <- as.matrix(rbind(com, x))
    z <- vegan::vegdist(y, method="jaccard", binary=TRUE, diag=TRUE, upper=TRUE)
    z <- as.matrix(z)
    return(z)
  }
  # Replicate.
  nulls.jac <- replicate(n.reps, jac_obs_null_chase(com, w))
  # Calculate means.
  mean.nulls.jac <- apply(nulls.jac, c(1:2), mean, na.rm=TRUE)
  n <- nrow(mean.nulls.jac)
  # Convert to a distance matrix.
  mean.nulls.jac <- as.dist(mean.nulls.jac)
  # Make a vector of groups.
  grp <- c(rep("Observed", n/2), rep("Null", n/2))
  # Test for difference in dispersion between original and randomized groups.
  rslt <- vegan::betadisper(mean.nulls.jac, grp, type="median", bias.adjust=TRUE)
  return(rslt)
}