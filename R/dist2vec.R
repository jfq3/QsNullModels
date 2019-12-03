#' Distance to Vector
#'
#' @param d A distance matrix.
#'
#' @return A vector of the distances.
#' @export
#'
#' @examples
#' data(dune)
#' d <- vegdist(dune, method = "bray)
#' dist2vec(d)
dist2vec <- function (d) {
  attributes(x) <- NULL
  d
}
