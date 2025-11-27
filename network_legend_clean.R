#' Network plot legend (clean, standalone)
#'
#' Draw a self-contained legend for a network plot where:
#' \itemize{
#'   \item Node size reflects total number of participants per treatment.
#'   \item Edge thickness reflects number of studies per comparison.
#' }
#'
#' This is designed to be used side-by-side with \code{netmeta::netgraph()},
#' e.g. in a multi-panel PDF or in a Shiny layout.
#'
#' @param col_nodes Fill colour for nodes.
#' @param col_edges Colour for edges.
#'
#' @return Called for its side-effect (drawing a plot).
#'
#' @importFrom graphics par plot.new text points segments
#' @export
network_legend_clean <- function(
  col_nodes = "orange",
  col_edges = "lightgray"
) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(1, 1, 1, 1))
  plot.new()

  ## NODE LEGEND
  text(0.05, 0.92, "Node size ~ number of participants", adj = 0, font = 2)

  node_cex <- c(2.2, 3.5, 5.5)
  node_labels <- c(
    "Fewer participants",
    "Intermediate number of participants",
    "More participants"
  )
  y_nodes <- c(0.78, 0.66, 0.54)

  for (i in seq_along(node_cex)) {
    points(0.12, y_nodes[i], pch = 21, bg = col_nodes, col = "black",
           cex = node_cex[i])
    text(0.22, y_nodes[i], node_labels[i], adj = 0, cex = 1.1)
  }

  ## EDGE LEGEND
  text(0.05, 0.38, "Edge thickness ~ number of studies", adj = 0, font = 2)

  edge_lwd <- c(1, 3, 6)
  edge_labels <- c("Fewer studies", "Moderate number of studies", "More studies")
  y_edges <- c(0.28, 0.22, 0.16)

  for (i in seq_along(edge_lwd)) {
    segments(0.10, y_edges[i], 0.30, y_edges[i],
             lwd = edge_lwd[i], col = col_edges)
    text(0.35, y_edges[i], edge_labels[i], adj = 0, cex = 1.1)
  }

  invisible(NULL)
}
