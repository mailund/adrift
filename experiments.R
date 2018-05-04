
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
# g$connect_nodes("x", "y")
# g$connect_nodes("y", "ap")
# g$connect_nodes("y", "b")
# g$connect_nodes("ap", "a")
# g$connect_nodes("x", "z")
# g$connect_nodes("u", "w")
# g$connect_nodes("w", "v")
# g$connect_nodes("w", "ap")

edges <- read_dot(readr::read_file("data-raw/Basic_OngeEA_wArch.dot"))
from_nodes <- llmap(edges, function(e) e["from"]) %>% as.vector()
to_nodes <- llmap(edges, function(e) e["to"]) %>% as.vector()
nodes <- c(from_nodes, to_nodes) %>% unique()
for (n in nodes) g$add_node(n)
llmap(edges, function(e) g$connect_nodes(e["from"], e["to"]))


graph <- tbl_graph(nodes = g$ggraph_nodes,
                   edges = g$ggraph_edges)
graph %>%
    ggraph(layout = ag_layout) +
    geom_node_text(aes(label = label)) +
    geom_edge_link() #+    theme_graph()
