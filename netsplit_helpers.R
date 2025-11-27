#' Extract full and significant node-splitting (netsplit) results
#'
#' Wrapper around \code{netmeta::netsplit()} that constructs a tidy
#' data frame with NMA, direct and indirect estimates (back-transformed),
#' RoR, z and p values, plus a filtered version with p < 0.05.
#'
#' @param net A \code{netmeta} object.
#' @param common Logical, if TRUE use common-effect results, otherwise random.
#'
#' @return A list with:
#'   \item{full}{Data frame with all comparisons.}
#'   \item{significant}{Subset of rows with \code{p < 0.05}.}
#'
#' @importFrom netmeta netsplit
#' @export
netsplit_data <- function(net, common) {
  nsp <- netmeta::netsplit(net, random = !common, common = common)

  df <- data.frame(
    comparison = nsp$comparison,
    nma = exp(if (common) nsp$common$TE else nsp$random$TE),
    direct = exp(if (common) nsp$direct.common$TE else nsp$direct.random$TE),
    indirect = exp(if (common) nsp$indirect.common$TE else nsp$indirect.random$TE),
    RoR = exp(if (common) nsp$compare.common$TE else nsp$compare.random$TE),
    z = if (common) nsp$compare.common$z else nsp$compare.random$z,
    p = if (common) nsp$compare.common$p else nsp$compare.random$p
  )

  list(
    full = df,
    significant = subset(df, p < 0.05)
  )
}

#' Compact node-splitting (netsplit) list output
#'
#' Generate a compact data frame listing comparison, p-values for
#' NMA, direct, and comparison (inconsistency) plus RoR.
#'
#' @param net A \code{netmeta} object.
#' @param common Logical, if TRUE use common-effect results, otherwise random.
#'
#' @return A data frame with columns:
#'   \code{comparison}, \code{p_nma}, \code{p_dir}, \code{p_comp}, \code{RoR}.
#'
#' @importFrom netmeta netsplit
#' @export
netsplit_list_data <- function(net, common) {
  nsp <- netmeta::netsplit(net, random = !common, common = common)
  data.frame(
    comparison = as.character(nsp$comparison),
    p_nma = if (common) nsp$common$p else nsp$random$p,
    p_dir = if (common) nsp$direct.common$p else nsp$direct.random$p,
    p_comp = if (common) nsp$compare.common$p else nsp$compare.random$p,
    RoR = exp(if (common) nsp$compare.common$TE else nsp$compare.random$TE)
  )
}
