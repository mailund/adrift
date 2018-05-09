
# FIXME: Don't know how to test plotting code
# nocov start

ag_layout <- function(g) function(graph, circular, ...) {
    g$layout()
    cbind(g$get_node_positions() %>% dplyr::select(x, y),
          graph, circular = NA)
}

#' Construct a graph plot object.
#'
#' This object can be modified using `ggraph` and `ggplot2` functions.
#'
#' @param graph An admixture/drift graph.
#' @param edge_colour The colour to use for the graph edges.
#' @param leaf_label_angle Angle to display leaf nodes in.
#'     Useful if they would otherwise overlap.
#' @return A graph object
#' @export
make_graph_plot <- function(
    graph,
    edge_colour = "darkblue",
    leaf_label_angle = 0,
    leaf_label_size = 4
) {
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
        ggraph::geom_edge_link(edge_width = 0.8,
                               edge_colour = edge_colour) +
        ggplot2::coord_cartesian(clip = "off") +
        ggraph::theme_graph()
}

#' Add ileaf labels
#'
#' @param plt Plot object created by [make_graph_plot()].
#' @return Updated plotting object
#' @export
show_leaf_labels <- function(plt, ...) {
    plt + ggraph::geom_node_text(ggplot2::aes(filter = is_leaf, label = label),
                                 ...)
}

#' Add inner nodes labels
#'
#' @param plt Plot object created by [make_graph_plot()].
#' @return Updated plotting object
#' @export
show_inner_node_labels <- function(plt, ...) {
    plt + ggraph::geom_node_label(ggplot2::aes(filter = !is_leaf, label = label),
                                  size = 3, nudge_y = -0.1, repel = TRUE,
                                  ...)
}

# nocov end
