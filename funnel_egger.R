#' Extract upper triangle of a symmetric matrix as a vector
#'
#' Helper used to extract treatment-effect estimates from a netmeta object
#' for Egger's regression.
#'
#' @param mat Numeric matrix (e.g. TE or seTE from a netmeta object).
#'
#' @return Numeric vector of the upper triangle (excluding diagonal).
#'
#' @export
extract_upper_triangle <- function(mat) {
  mat[lower.tri(mat, diag = TRUE)] <- NA
  as.vector(mat)
}

#' Compute Egger's regression test from a netmeta object
#'
#' Use the upper triangle of the treatment-effect (TE) and corresponding
#' standard error matrices from a \code{netmeta} object to run Egger's
#' regression test using \code{meta::metagen()} and \code{meta::metabias()}.
#'
#' @param net A \code{netmeta} object.
#' @param model Character, \code{"random"} or \code{"fixed"} to choose
#'   between TE.random/seTE.random and TE.fixed/seTE.fixed.
#'
#' @return A \code{metabias} object, or \code{NULL} if fewer than 10
#'   comparisons are available.
#'
#' @importFrom meta metagen metabias
#' @export
compute_egger <- function(net, model = "random") {
  if (model == "random") {
    TE_mat <- net$TE.random
    se_mat <- net$seTE.random
  } else {
    TE_mat <- net$TE.fixed
    se_mat <- net$seTE.fixed
  }

  vec_TE <- extract_upper_triangle(TE_mat)
  vec_se <- extract_upper_triangle(se_mat)

  idx <- which(!is.na(vec_TE) & !is.na(vec_se))
  vec_TE <- vec_TE[idx]
  vec_se <- vec_se[idx]

  if (length(vec_TE) < 10L) {
    return(NULL)
  }

  mg <- meta::metagen(TE = vec_TE, seTE = vec_se, sm = "SMD")
  meta::metabias(mg)
}
