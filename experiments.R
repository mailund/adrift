
library(ggplot2)

g <- new(Graph)
g$add_node("x") ; g$add_node("y") ; g$add_node("z") ; g$add_node("u") ; g$add_node("v")
g$nodes

g$connect_nodes("x", "y")
g$connect_nodes("x", "z")
g$connect_nodes("u", "x")
g$connect_nodes("u", "v")
stopifnot(g$connected)

g$randomize_node_positions()
g$node_positions

plot_g <- function(g) {
    g$node_positions %>%
        ggplot(aes(x = x, y = y, label = label)) +
        geom_text()
}

plot_g(g)
drags <- 1/rep(1:100, each = 10)
for (drag in drags) {
    g$force_step(drag)
}
plot_g(g)
