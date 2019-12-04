#' Distance to Vector
#' 
#' Converts a distance object into a vector.
#' 
#' @usage dist2vec(d)
#' @param d A distance matrix.
#' @return A vector of the distances.
#' @export
#' @description This function converts a distance object into a vector. The vectors may then
#'   be incorporated into a summary table.
#' @author John Quensen
#' @importFrom vegan vegdist
#' @examples
#' data(dune)
#' d <- vegdist(dune, method = "bray")
#' dist2vec(d)
#' 
dist2vec <- function (d)
{
  attributes(d) <- NULL
  d
}
