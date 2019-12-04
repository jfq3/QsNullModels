#' Mean Shared Species
#' 
#' Calculates the mean number of shared species from a community matrix for all sample pairs.
#' 
#' @usage mean_shared_spp(com)
#' @param com A community matrix with samples as rows and species as columns.
#'
#' @return A real number.
#' @export
#' @author John Quensen
#' @details If presented with a community matrix of count data, this function first converts 
#'   it to presence/absence.
#' @examples
#' data(com)
#' mean_shared_spp(com)
#' @importFrom vegan decostand

mean_shared_spp <- function(com){
  com <- as.matrix(com)
  com <- decostand(com, "pa")
  return(mean(shared_spp(com)))
}