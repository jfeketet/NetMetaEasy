# netforest_plot.R
# --------------------------------------------------


netmeta_forest_plot <- function(
  net,
  reference = NULL,
  rank = FALSE,
  label_left  = NULL,
  label_right = NULL,
  small_values = "desirable",
  xlim = NULL,
  model = c("random", "fixed"),
  fontsize = 12
) {

  model <- match.arg(model)

  if (is.null(net))
    stop("netmeta_forest_plot: 'net' object is NULL.")

  # Define small/large effect direction
  small_values <- match.arg(
    small_values,
    choices = c("desirable", "undesirable")
  )

  # Determine TE sorting if rank = TRUE
  if (rank) {
    if (is.null(net$Pscore))
      stop("Rank plot requested but P-score not available.")
    sortvar <- -net$Pscore
  } else {
    sortvar <- NULL
  }

  # Forest arguments
  args <- list(
    net,
    label.left  = label_left,
    label.right = label_right,
    fontsize = fontsize,
    ref = reference,
    small.values = small_values,
    drop.reference.group = FALSE
  )

  if (!is.null(sortvar))
    args$sortvar <- sortvar

  # xlim handling
  if (!is.null(xlim)) {
    args$xlim <- xlim
  }

  # model selection
  if (model == "fixed") {
    args$common <- TRUE
    args$random <- FALSE
  } else {
    args$common <- FALSE
    args$random <- TRUE
  }

  # draw forest
  do.call(forest, args)
}