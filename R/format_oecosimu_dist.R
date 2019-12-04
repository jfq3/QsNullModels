#' Reformat an oecosimu Object
#' 
#' Reformats an oecosim object if the statistic returned is a distance matrix.
#' 
#' @usage format_oecosimu_dist(sol, com)
#' @param sol Result of oecosimu when statistic is a matrix.
#' @param com Community matrix used to calculate oecosimu result.
#' @details Vegan's oecosimu function is used to compare an observed and expected value or
#'   values based on a null model. A drawback of the oecosimu summary output is that sample 
#'   pairs are not named.  
#'   
#'   This function reformats the oecosimu result into a table giving the names of sample
#'   pairs, the observed statistic, the expected statistic and its standard deviation based 
#'   on the null model, the standard effect size, the selection strength as defined in Zhou 
#'   et al. (2014), and the probability that the observed and expected values of the statistic
#'   are not different. This function is appropriate when the statistic is a distance matrix.
#' 
#' @return A data frame.
#' @export
#' @author John Quensen
#' @seealso format_oecosimu_vector
#' @references Zhou JZ, Deng Y, Zhang P et al. (2014) Stochasticity, succession, and environmental
#'   perturbations in a fluidic ecosystem. Proceedings of the National Academy of Sciences of the
#'   United States of America, 111, E836-E845.
#' @importFrom stats sd
format_oecosimu_dist <- function(sol, com) {
  stat.obs <- as.vector(sol$oecosim$statistic)
  null.mean <- as.vector(sol$oecosim$means)
  null.sd <- apply(sol$oecosim$simulated, 1, sd, na.rm = TRUE)
  SES <- as.vector(sol$oecosim$z)
  SS <- (stat.obs-null.mean)/stat.obs
  p.val <- sol$oecosim$pval
  n.lines <- length(stat.obs)
  site.1 <- site.2 <- rep("", n.lines)
  N <- nrow(com)
  k <- 1
  for (i in 1:(N-1)) {
    for (j in (i+1):N ){
      # pairs[k] <- paste(rownames(com)[i], '_', rownames(com)[j], sep="")
      site.1[k] <- rownames(com)[i]
      site.2[k] <- rownames(com)[j]
      k <- k+1
    }
  }
  
  result <- data.frame(site.1, site.2, stat.obs, null.mean, null.sd, SES, SS, p.val)
  return(result)
}
