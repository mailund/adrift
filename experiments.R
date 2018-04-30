
text <- readr::read_file("data-raw/Basic_OngeEA_wArch.dot")

edges <- dot_parse_graph(text)
edges_tbl <- dot_get_edges(edges)
edges_df <- as_tibble(edges_tbl)

count_in <- edges_df %>%
    group_by(to) %>%
    mutate(in_degree = n()) %>%
    ungroup() %>%
    select(to, in_degree) %>%
    rename(node = to)

count_out <- edges_df %>%
    group_by(from) %>%
    mutate(out_degree = n()) %>%
    ungroup() %>%
    select(from, out_degree) %>%
    rename(node = from)

degrees <- full_join(count_in, count_out, by = "node") %>%
    mutate(in_degree = ifelse(is.na(in_degree), 0, in_degree),
           out_degree = ifelse(is.na(out_degree), 0, out_degree))

leaves <- degrees %>%
    filter(out_degree == 0) %>%
    select(node) %>% .[[1]] %>% unique()

inner_nodes <- degrees %>%
    filter(out_degree > 0) %>%
    select(node) %>% .[[1]] %>% unique()

admixture_nodes <- degrees %>%
    filter(in_degree == 2) %>%
    select(node) %>% .[[1]] %>% unique()

edges_tbl[,1:3] <- edges_tbl[,c(2,1,3)]
edges_tbl[,3] <- NA
colnames(edges_tbl) <- c("child", "parent", "prop")

library(tidygraph)
library(ggraph)
nodes <- data.frame(label = c(leaves, inner_nodes))
edges <- data.frame(from = edges_tbl[,"parent"], to = edges_tbl[,"child"],
                    stringsAsFactors = FALSE)
graph <- tbl_graph(nodes, edges)

ag_layout <- function(graph, circular, ...) {
    print(graph)
    nodes <- with_graph(graph, .N())
    n <- nrow(nodes)
    cbind(data.frame(x = 1:n, y = 1:n, circular = NA), graph)
}

xx <- graph %>%
    ggraph(layout = ag_layout) +
    geom_node_text(aes(filter = (label %in% leaves), label = label)) +
    geom_edge_link() +
    theme_graph()
xx


x <- "foo"
y <- x

attr(x, "foo") <- "bar"
x
y
