#' Reformat an oecosimu Vector
#' 
#' @name format_oecosimu_vector
#' @aliases format_oecosimu_vector
#' @description Reformat an oecosimu Vector
#' @param sol Result of oecosimu when statistic is a vector.
#' @param com Community matrix used to calculate oecosimu result.
#'
#' @return A data frame.
#' @export
#' @details Vegan's oecosimu function is used to compare an observed and expected value or values based on a null model. A drawback of the oecosimu summary output is that sample pairs are not named. This function reformats the oecosimu result into a table giving the names of sample pairs, the observed statistic, the expected statistic and its standard deviation based on the null model, the standard effect size, and th probability that the observed and expected values of the statistic are not different. This function is appropriate when the statistic is a vector rather than a distance matrix.
#' @author John Quensen
#' @seealso format_oecosimu_dist
#' @importFrom stats sd
format_oecosimu_vector <- function(sol, com) {
  stat.obs <- sol$oecosim$statistic
  null.mean <- sol$oecosim$means
  null.sd <- apply(sol$oecosim$simulated, 1, sd, na.rm = TRUE)
  SES <- sol$oecosim$z
  p.val <- sol$oecosim$pval
  
  n.lines <- length(sol$statistic)
  site.1 <- site.2 <- vector(mode="character", length=n.lines)
  N <- nrow(com)
  k <- 1
  for (i in 1:(N-1)) {
    for (j in (i+1):N ){
      #pairs[k] <- paste(rownames(com)[i], "_", rownames(com)[j], sep="")
      site.1[k] <- rownames(com)[i]
      site.2[k] <- rownames(com)[j]
      k <- k+1
    }
  }
  
  result <- data.frame(site.1, site.2, stat.obs, null.mean, null.sd, SES, p.val)
  return(result)
}
