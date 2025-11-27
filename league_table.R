#' Generate league table for a netmeta object
#'
#' Thin wrapper around \code{netmeta::netleague()} to extract the
#' random or fixed-effect league table.
#'
#' @param netobj A \code{netmeta} object.
#' @param backtransf Logical, passed to \code{netleague()}.
#' @param digits Integer, number of digits to display.
#' @param model Character, either \code{"random"} or \code{"fixed"}.
#'
#' @return A matrix (or data frame) containing the league table.
#'
#' @importFrom netmeta netleague
#' @export
generate_league_table <- function(netobj,
                                  backtransf = TRUE,
                                  digits = 2,
                                  model = "random") {
  tbl <- netmeta::netleague(netobj, digits = digits, backtransf = backtransf)
  if (model == "random") {
    tbl$random
  } else {
    tbl$common
  }
}

#' Mark significant cells in a league table (HTML)
#'
#' Given a single cell from a league table (as character), detect confidence
#' intervals and mark statistically significant entries (where the CI does not
#' cross a given null value) with a red asterisk in HTML.
#'
#' @param cell Character cell content, e.g. \code{"1.45 [0.90; 2.36]"}.
#' @param null_val Numeric null value (1 for ratios, 0 for differences).
#'
#' @return Character containing the possibly decorated HTML version.
#'
#' @export
mark_significance <- function(cell, null_val) {
  if (is.na(cell) || !grepl("\\[", cell)) {
    return(cell)
  }

  ci <- regmatches(cell,
                   regexec("([-0-9\\.]+)\\s*\\[\\s*([-0-9\\.]+)\\s*;\\s*([-0-9\\.]+)\\s*\\]",
                           cell))[[1]]
  if (length(ci) != 4) {
    return(cell)
  }

  lower <- as.numeric(ci[3])
  upper <- as.numeric(ci[4])

  if (!is.na(lower) && !is.na(upper) &&
      (lower > null_val || upper < null_val)) {
    return(paste0(
      "<b><span style='color:red;' title='Statistically significant (p < 0.05)'>",
      cell, " *</span></b>"
    ))
  }

  cell
}
