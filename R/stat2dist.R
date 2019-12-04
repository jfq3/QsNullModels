#' Convert Vector to Distance
#' 
#' This function converts a vector of distances into a distance matrix.
#' 
#' @usage stat2dist(com, stat, diagonal = TRUE)
#' @param com The community matrix for which the vector of distances was calculated.
#' @param stat One of the statistics from the ecosimu function: statistic, mean.
#' @param diagonal A logical determining if distance matrix is square or without upper  half and diagonal.
#' @return A distance matrix.
#' @export
#' @details The oecosimu function in vegan may be used to calculate the observed distances
#'   for a community matrix and the expected distances based on a null model, but the output
#'   is in the form of vectors. This function converts such vectors into a distance object 
#'   that may be used for ordination or clustering of samples. In the oecosimu output, 
#'   statistic is the observed value and mean is the expected value based on the null model.
#' @author John Quensen
stat2dist <- function(com, stat, diagonal=TRUE) {
  # Convert a stat vector from oecosimu result to the same format as a distance matrix.
  # com is the community matrix used with the oecosimu function.
  # stat is one of the statistics from the ecosimu function: statistic, z, mean
  # diagonal is a logical determining if distance matrix is square or without lower half
  stat.d <- matrix(NA, nrow(com), nrow(com))
  k=1
  for (j in 1:(nrow(com)-1)) {
    for (i in ((j+1):nrow(com))) {
      stat.d[i,j] <- stat[k]
      k=k+1
    }
  }
  if (diagonal) {
    stat.d <- as.dist(stat.d, diag=FALSE, upper=FALSE)
  }
  else {
    stat.d <- as.dist(stat.d, diag=TRUE, upper=TRUE)
  }
  attrib.list <- attributes(stat.d)
  attrib.list$Labels <- rownames(com)
  attrib.list$method <- "stat2dist"
  attributes(stat.d) <- attrib.list
  return(stat.d)
}
