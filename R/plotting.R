
# FIXME: Don't know how to test plotting code
# nocov start

ag_layout <- function(g) function(graph, circular, ...) {
    g$layout()
    node_positions <- with(
        g$get_node_positions(),
        data.frame(x = x, y = y)
    )
    cbind(node_positions, graph, circular = NA)
}

#' Construct a graph plot object.
#'
#' This object can be modified using `ggraph` and `ggplot2` functions.
#'
#' @param graph An admixture/drift graph.
#' @param ... Parameters passed to [ggraph::geom_edge_link()] that you can
#'            use to customise the edges.
#' @return A graph object
#' @export
make_graph_plot <- function(graph, ...) {
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        stop("Plotting require the ggraph package, which is not installed")
    }
    if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Plotting require the tidygraph package, which is not installed")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE,
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

#' Add leaf labels.
#'
#' This is a wrapper around [ggraph::geom_node_text()] and you can customise
#' the plotting through `...`
#'
#' @param plt Plot object created by [make_graph_plot()].
#' @param ... Parameter forwarded to [ggraph::geom_node_text()].
#' @return Updated plotting object
#' @export
show_leaf_labels <- function(plt, ...) {
    is_leaf <- label <- NULL # to satisfy CMD CHECK
    plt + ggraph::geom_node_text(ggplot2::aes(filter = is_leaf, label = label),
                                 ...)
}

#' Add inner nodes labels.
#'
#' This is a wrapper around [ggraph::geom_node_label()] and you can use
#' `...` to customise the plot.
#'
#' @param plt Plot object created by [make_graph_plot()].
#' @param ... Parameter forwarded to [ggraph::geom_node_label()].
#' @return Updated plotting object
#' @export
show_inner_node_labels <- function(plt, ...) {
    is_leaf <- label <- NULL # to satisfy CMD CHECK
    plt + ggraph::geom_node_label(ggplot2::aes(filter = !is_leaf, label = label),
                                  size = 3, nudge_y = -0.1, repel = TRUE,
                                  ...)
}

# nocov end
