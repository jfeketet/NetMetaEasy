#' Compute total sample size per treatment
#'
#' Given a study-level data frame and a netmeta object, sum up the total
#' number of participants per treatment (treat1, treat2, treat3 + n1, n2, n3).
#'
#' @param data_df Data frame with columns \code{treat1}, \code{treat2}
#'   (optionally \code{treat3}) and corresponding \code{n1}, \code{n2},
#'   \code{n3} where available.
#' @param net A \code{netmeta} object (used to detect treatments).
#'
#' @return Named numeric vector of total sample sizes per treatment.
#'
#' @export
compute_treatment_counts <- function(data_df, net) {
  trts <- net$trts
  counts <- stats::setNames(rep(0, length(trts)), trts)

  # generic: iterate treat/n columns
  trt_cols <- grep("^treat", names(data_df), value = TRUE)
  n_cols   <- gsub("treat", "n", trt_cols)

  for (i in seq_len(nrow(data_df))) {
    for (k in seq_along(trt_cols)) {
      if (!n_cols[k] %in% names(data_df)) next
      tname <- as.character(data_df[[trt_cols[k]]][i])
      nval  <- data_df[[n_cols[k]]][i]
      if (!is.na(tname) && !is.na(nval) && tname %in% names(counts)) {
        counts[tname] <- counts[tname] + nval
      }
    }
  }
  counts
}

#' Compute node sizes (cex) from treatment counts
#'
#' Map treatment-specific sample sizes to \code{cex.points} values
#' for \code{netmeta::netgraph()}, using a linear scaling between
#' 4 and 10.
#'
#' @param counts Named numeric vector of sample sizes per treatment.
#'
#' @return Numeric vector of \code{cex} values in the same order as \code{counts}.
#'
#' @export
compute_node_cex <- function(counts) {
  pos <- counts[counts > 0]
  if (length(pos) == 0L) {
    return(rep(6, length(counts)))
  }

  minv <- min(pos, na.rm = TRUE)
  maxv <- max(pos, na.rm = TRUE)

  sapply(counts, function(n) {
    if (is.na(n) || n <= 0) return(4)
    if (maxv == minv) return(7)
    scaled <- (n - minv) / (maxv - minv)
    4 + scaled * (10 - 4)
  })
}

#' Draw network graph with node sizes scaled by sample size
#'
#' Convenience wrapper around \code{netmeta::netgraph()} that first computes
#' total sample size per treatment and derives appropriate \code{cex.points}.
#'
#' @param net A \code{netmeta} object.
#' @param data_df Data frame with treatment and n columns (see
#'   \code{\link{compute_treatment_counts}}).
#' @param ... Further arguments passed to \code{netmeta::netgraph()}.
#'
#' @return Invisibly, the result of \code{netmeta::netgraph()}.
#'
#' @importFrom netmeta netgraph
#' @export
draw_netgraph_with_sizes <- function(net, data_df, ...) {
  counts <- compute_treatment_counts(data_df, net)
  cex_points <- compute_node_cex(counts)

  netmeta::netgraph(
    net,
    cex.points = cex_points,
    ...
  )
}
