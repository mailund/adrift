
# FIXME: Don't know how to test plotting code
# nocov start

ag_layout <- function(g) function(graph, circular, ...) {
    node_positions <- with(g$get_node_positions(), data.frame(x = x, y = y))
    g$layout()
    cbind(node_positions,
          graph, circular = NA)
}

#' Construct a graph plot object.
#'
#' Parameters in `...` will be forwarded to [ggraph::geom_edge_link()].
#' The returned object can be modified using `ggraph` and `ggplot2` functions.
#'
#' @param graph An admixture/drift graph.
#' @param ... Parameters that will be forwarded to [ggraph::geom_edge_link()].
#' @return A graph plot object.
#' @export
make_graph_plot <- function(graph, ...) {
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        stop("Plotting require the ggraph package, which is not installed")
    }
    if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Plotting require the tidygraph package, which is not installed")
    }
    if (!requireNamespace("ggplot2",, quietly = TRUE,
                          versionCheck = list(op = ">=", version = "2.2.1.9000")
    )) {
        stop("Plotting require package ggplot2 (>= 2.2.1.9000)")
    }

    tidygraph::tbl_graph(nodes = graph$get_ggraph_nodes(),
                         edges = graph$get_ggraph_edges()) %>%
        ggraph::ggraph(ag_layout(graph)) +
        ggraph::geom_edge_link(edge_width = 0.8, ...) +
        ggplot2::coord_cartesian(clip = "off") +
        ggraph::theme_graph()
}

#' Add leaf labels to a graph plot.
#'
#' This function is a wrapper around [ggraph::geom_node_text()], so
#' you can modify the plot using `...` that will be forwarded to that
#' function.
#'
#' @param plt Plot object created by [make_graph_plot()].
#' @param ... Parameters that will be forwarded to [ggraph::geom_node_text()].
#' @return Updated plotting object
#' @export
show_leaf_labels <- function(plt, ...) {
    plt + ggraph::geom_node_text(ggplot2::aes_(filter = "is_leaf",
                                               label = "label"),
                                 ...)
}

#' Add inner nodes labels
#'
#' This function is a wrapper around [ggraph::geom_node_label()], so
#' you can modify the plot using `...` that will be forwarded to that
#' function.
#'
#' @param plt Plot object created by [make_graph_plot()].
#' @param ... Parameters that will be forwarded to [ggraph::geom_node_label()].
#' @return Updated plotting object.
#' @export
show_inner_node_labels <- function(plt, ...) {
    # to silence CMD CHECK notes
    is_leaf <- label <- NULL
    plt + ggraph::geom_node_label(ggplot2::aes(filter = !is_leaf,
                                               label = label),
                                  size = 3, nudge_y = -0.1, repel = TRUE,
                                  ...)
}

# nocov end
