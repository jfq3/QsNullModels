#' My Modification of Chase's Raup-Crick
#' 
#' Calculates a modified version of the Raup-Crick metric by Chase's method, but using a faster R script. Some of Chase's original options are omitted, and defaults are different.
#' 
#' @aliases my_rc
#' @usage my_rc(com, classic_metric = TRUE, split_ties = TRUE, 
#'   nreps = 999, set_all_species_equal = FALSE)
#'   
#' @param com A community matrix with samples as rows and species as columns.
#' @param classic_metric A logical specifying whether or not the Raup-Crick 
#'   metric should be standardized to the range -1 to 1 as Chase did.
#'   Defaults to TRuE.
#' @param split_ties A logical specifying how ties are handled in calculating probabilities. Defaults to TRUE.
#' @param nreps The number of replications used in calculating null expectations..
#' @param set_all_species_equal A logical specifying sampling probablilites. Defaults to FALSE.
#'
#' @return A Raup-Crick distance matrix.
#' @export
#'
#' @details This function calculates a modified version of Raup-Crick using a null model
#'   approach. By default, it returns a distance matrix apppropriate for ordination.  
#'   If classic_metric is set to FALSE, the metric is standardized as Chase preferred to 
#'   range from -1 to 1 instead of 0 to 1. When this is done, values near -1 indicate that 
#'   samples are more similar than expected by chance and values near 1 indicate that samples
#'   are less similar than expected by chance. If the result is to be used as a distance
#'   method, the metric should not be re-scaled (keep classic_metric set to TRUE, the default
#'   value).  
#'   If ties are split (as Chase recommends) the dissimilarity matrix can be flipped by 
#'   multiplying by -1 (for Chase's modification, which ranges from -1 to 1) or by subtracting 
#'   the metric from 1 (for the classic metric which ranges from 0 to 1). If ties are not split
#'   (and there are ties between the observed and expected shared number of species) this 
#'   conversion will not work. 
#'   The argument nreps specifies the number of randomizations; a minimum of 999 is recommended.  
#'   If the argument set_all_species_equal is set to TRUE, all species are weighted equally in
#'   the null model; this is not recommended. Chase's preferred method is to weight species
#'   by frequency of occupancy, as occurs when set_all_species_equal is left at the default 
#'   value FALSE.
#'   Raup-Crick is calculated from a presence/absence communitiy matrix. If this function is 
#'   given count data, it first converts it to presence/absence.  
#'   The choice of how many plots (rows) to include has a real impact  on the metric, as species
#'   and their occurrence frequencies across the set of plots is used to determine gamma and 
#'   the frequency with which each species is drawn from the null model.
#' 
#' @return With classic_metric=TRUE, returns a distance matrix that may be used for 
#'   ordination or clustering.  With classic_metric=FALSE, returns a matrix re-scaled
#'   to the interval -1 to 1.
#' @author John Quensen
#' @references Chase, J. M., N. J. B. Kraft, K. G. Smith, M. Vellend, and B. D. Inouye. 
#'   2011. Using null models to disentangle variation in community dissimilarity from variation 
#'   in alpha-diversity. Ecosphere 2(2): Article 24.  
#'   Raup, D. M. and R. E. Crick. 1979. Measurement of faunal similarity in paleontology. 
#'   Journal of Paleontology 53:1213-1227.
#'   
#' @examples
#' data(com)
#' dist.rc <- my_rc(com)
#'
my_rc <- function(com, classic_metric=TRUE, split_ties=TRUE, nreps=999, set_all_species_equal = FALSE) {
  # Make sure com is binary
  com <- as.matrix(com)
  com <- ifelse(com > 0, 1, 0)
  # Define function to replicate.
  if (set_all_species_equal==FALSE) {
    w <- colSums(com)
  }
  else {
    w <- rep(1, ncol(com))
  }
  N <- nrow(com)
  d <- (N*(N-1)/2) # number of pairwise sample combinationas
  tri <- matrix(FALSE, N, N)
  tri <- row(tri) > col(tri)
  ss <-  function(x) {tcrossprod(x)[tri]}
  ss.reps <- function(x, w) {ss(randomize_by_rows(x, w))}
  # Replicate
  nulls <- replicate(nreps, ss.reps(com, w))
  # Calculate probabilities
  obs <- ss(com)
  obs.nulls <- cbind(obs, nulls)
  prob <- mat.or.vec(nr=d, nc=1)
  # If classic_metric=TRUE and split_ties=TRUE
  if (split_ties==TRUE) {
    for (i in 1:d) {
      prob[i] <- 1-(sum(obs.nulls[i,]<obs[i]) + (sum(obs.nulls[i,]==obs[i]))/2)/(nreps+1)
    }
  }
  # If classic_metric=TRUE and split_ties=FALSE
  else {
    for (i in 1:d) {
      prob[i] <- sum(obs.nulls[i,]>obs[i])/(nreps+1)
    }
  }
  # If classic_metric=EALSE
  if (classic_metric==FALSE) {
    prob <- (prob-0.5)*2
  }
  # Convert to distance matrix
  rc <- stat2dist(com, prob, diagonal=TRUE)
  # Return
  return(rc)
}
