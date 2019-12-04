#' Simberloff Summary
#' 
#' Makes a table of the number of times observed shared species are greater or lesser
#'   than expected, and number of times significantly so.
#' 
#' @aliases simberloff_sum
#' @usage simberloff_sum(rslt.sum, alpha = 0.05)
#' @param rslt.sum Result from function format_oecosimu_vector.
#' @param alpha Confidence level.
#' @return A data frame.
#' @export
#' @references Simberloff, D. 1978. Using island biogeographic distributions to determine
#'   if colonizatioon is stochastic. American Naturalist 112:713-726.
#' @author John Quensen
#' @seealso format_oecosimu_vector
simberloff_sum <- function(rslt.sum, alpha=0.05) {
  e <- rslt.sum$SES==0
  g <- rslt.sum$SES > 0
  l <- rslt.sum$SES<0
  a.max <- 1-(alpha/2)
  a.min <- alpha/2
  sig <- (rslt.sum$p.val>a.max)|(rslt.sum$p.val<a.min)
  
  col.a <-  c("Observed equals Expected", "Observed > Expected", 
              "Significant(Observed > Expected)", 
              "Observed < Expected", "Significant(Observed < Expected)")
  col.b <- c(sum(e), sum(g), sum(g&sig), sum(l), sum(l&sig))
  
  simberloff.sum <- data.frame(col.a, col.b)
  colnames(simberloff.sum) <- c("Case", "Occurrences")
  return(simberloff.sum)
}