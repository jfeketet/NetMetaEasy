#' Build a netmeta model from pairwise data
#'
#' Convenience wrapper to build a \code{netmeta} model for binary,
#' continuous or time-to-event data from a wide-format study-level
#' dataset with up to three arms.
#'
#' @param data_df Data frame containing the study-level data.
#'   Must include columns like \code{treat1}, \code{treat2} (and optionally
#'   \code{treat3}), plus the corresponding outcome columns depending on
#'   the summary measure (e.g. \code{event1}, \code{n1} etc.).
#' @param design Integer, 2 for three-arm design (1–2–3), 1 for
#'   standard two-arm trials.
#' @param summary_measure Character, summary measure (e.g. "RR", "OR",
#'   "MD", "SMD", "HR").
#' @param model_type Character, either \code{"random"} or \code{"fixed"}.
#' @param reference_group Optional character, reference treatment name.
#' @param ordered_trts Optional character vector, desired treatment order.
#' @param estimator Character, tau estimator passed to \code{netmeta}
#'   (e.g. "DL", "REML").
#'
#' @return A list with components:
#'   \item{pw}{The pairwise object used as input.}
#'   \item{net}{The fitted netmeta object.}
#'
#' @importFrom netmeta netmeta pairwise
#' @export
build_netmeta_model <- function(
  data_df,
  design,
  summary_measure,
  model_type,
  reference_group = NULL,
  ordered_trts = NULL,
  estimator = "DL"
) {
  # Build pairwise data object
  if (design == 2) {
    # three-arm (or multi-arm truncated to 3)
    pw <- netmeta::pairwise(
      treat = list(treat1, treat2, treat3),
      n     = if ("n1" %in% names(data_df))
        list(n1, n2, n3) else NULL,
      event = if ("event1" %in% names(data_df))
        list(event1, event2, event3) else NULL,
      mean  = if ("mean1" %in% names(data_df))
        list(mean1, mean2, mean3) else NULL,
      sd    = if ("sd1" %in% names(data_df))
        list(sd1, sd2, sd3) else NULL,
      TE    = if ("TE1" %in% names(data_df))
        list(TE1, TE2, TE3) else NULL,
      seTE  = if ("seTE1" %in% names(data_df))
        list(seTE1, seTE2, seTE3) else NULL,
      data = data_df,
      studlab = if ("studlab" %in% names(data_df))
        studlab else paste(data_df$Author, data_df$Year),
      sm = summary_measure
    )
  } else {
    # standard two-arm
    pw <- netmeta::pairwise(
      treat = list(treat1, treat2),
      n     = if ("n1" %in% names(data_df))
        list(n1, n2) else NULL,
      event = if ("event1" %in% names(data_df))
        list(event1, event2) else NULL,
      mean  = if ("mean1" %in% names(data_df))
        list(mean1, mean2) else NULL,
      sd    = if ("sd1" %in% names(data_df))
        list(sd1, sd2) else NULL,
      TE    = if ("TE1" %in% names(data_df))
        list(TE1, TE2) else NULL,
      seTE  = if ("seTE1" %in% names(data_df))
        list(seTE1, seTE2) else NULL,
      data = data_df,
      studlab = if ("studlab" %in% names(data_df))
        studlab else paste(data_df$Author, data_df$Year),
      sm = summary_measure
    )
  }

  args <- list(
    pw,
    random = (model_type == "random"),
    common = (model_type == "fixed"),
    method.tau = estimator,
    is.pairwise = TRUE
  )

  if (!is.null(reference_group) && nzchar(reference_group)) {
    args$reference.group <- reference_group
  }

  if (!is.null(ordered_trts) && length(ordered_trts) > 0) {
    args$seq <- ordered_trts
  }

  net <- do.call(netmeta::netmeta, args)

  list(pw = pw, net = net)
}
