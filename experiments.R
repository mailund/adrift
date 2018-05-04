
library(ggplot2)
library(tidygraph)
library(ggraph)
library(matchbox)

ag_layout <- function(graph, circular, ...) {
    g$layout()
    cbind(g$node_positions, circular = NA, graph)
}

g <- new(Graph)

# g$add_node("x") ; g$add_node("y") ; g$add_node("z") ;
# g$add_node("u") ; g$add_node("v") ; g$add_node("w")
# g$add_node("ap") ; g$add_node("a") ; g$add_node("b")
# g$nodes
#
# g$connect_nodes("u", "x")
# g$connect_nodes("u", "x")
# g$connect_nodes("x", "y")
# g$connect_nodes("y", "ap")
# g$connect_nodes("y", "b")
# g$connect_nodes("ap", "a")
# g$connect_nodes("x", "z")
# g$connect_nodes("u", "w")
# g$connect_nodes("w", "v")
# g$connect_nodes("w", "ap")

edges <- read_dot(readr::read_file("data-raw/Basic_OngeEA_wArch.dot"))
#edges <- read_dot(readr::read_file("data-raw/BosGraph.dot"))
from_nodes <- edges[,"parent"]
to_nodes <- edges[,"child"]
nodes <- c(from_nodes, to_nodes) %>% unique()
for (n in nodes) g$add_node(n)
for (i in seq_along(edges[,"parent"])) {
    g$connect_nodes(edges[i,"parent"], edges[i,"child"])
}

graph <- tbl_graph(nodes = g$ggraph_nodes,
                   edges = g$ggraph_edges)
graph %>%
    ggraph(ag_layout) +
    geom_edge_link(edge_width = 0.8, edge_colour = "darkblue") +
    geom_node_text(aes(filter = is_leaf, label = label),
                   size = 4, nudge_y = -0.3, angle = -20) +
    geom_node_label(aes(filter = !is_leaf, label = label),
                    size = 3, nudge_y = -0.1, repel = TRUE) +
    theme_graph()

