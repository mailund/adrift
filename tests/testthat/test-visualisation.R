context("Visualisation")

# It is hard testing the visualisation but we can test some of the information
# that we extract as part of the visualisation. Some is stochastic, though, so
# we do not test that.

make_tree <- function() {
    g <- new(Graph)
    g$add_node("a") ; g$add_node("b") ; g$add_node("c")
    g$add_node("d") ; g$add_node("e")
    g$connect_nodes("d", "a") ; g$connect_nodes("d", "b")
    g$connect_nodes("e", "d") ; g$connect_nodes("e", "c")
    return(g)
}

make_graph <- function() {
    g <- new(Graph)
    g$add_node("a") ; g$add_node("b") ; g$add_node("c")
    g$add_node("d") ; g$add_node("e") ; g$add_node("f") ; g$add_node("g")
    g$connect_nodes("d", "b")
    g$connect_nodes("e", "a") ; g$connect_nodes("e", "d")
    g$connect_nodes("f", "d") ; g$connect_nodes("f", "c")
    g$connect_nodes("g", "e") ; g$connect_nodes("g", "f")
    return(g)
}

test_that("we do not attempt to layout a graph that is not connected", {
    g <- new(Graph)
    g$add_node("a") ; g$add_node("b")
    expect_error(g$layout(), "The graph needs to be connected.*")
})

test_that("we get the right node information", {
    tree <- make_tree()
    node_info <- tree$get_ggraph_nodes()
    expect_equal(node_info$label, tree$get_nodes())
    expect_equal(node_info$is_leaf, c(TRUE, TRUE, TRUE, FALSE, FALSE))

    graph <- make_graph()
    node_info <- graph$get_ggraph_nodes()
    expect_equal(node_info$label, graph$get_nodes())
    expect_equal(node_info$is_leaf,
                 c(TRUE, TRUE, TRUE, # a b c
                   FALSE, FALSE, FALSE, FALSE)) # d e f g

})

library(dplyr)
edge_exists <- function(edge_info, from, to) {
    edges <- edge_info %>% filter(from == !!from, to == !!to)
    nrow(edges) == 1
}

test_that("we get the right edge information", {
    tree <- make_tree()
    edge_info <- tree$get_ggraph_edges()
    expect_true(edge_info %>% edge_exists("d", "a"))
    expect_true(edge_info %>% edge_exists("d", "b"))
    expect_false(edge_info %>% edge_exists("d", "c"))
    expect_true(edge_info %>% edge_exists("e", "d"))
    expect_true(edge_info %>% edge_exists("e", "c"))
    expect_false(edge_info %>% edge_exists("e", "a"))
    expect_false(edge_info %>% edge_exists("e", "b"))

    graph <- make_graph()
    edge_info <- graph$get_ggraph_edges()
    expect_true(edge_info %>% edge_exists("d", "b"))
    expect_true(edge_info %>% edge_exists("e", "a"))
    expect_true(edge_info %>% edge_exists("e", "d"))
    expect_true(edge_info %>% edge_exists("f", "d"))
    expect_true(edge_info %>% edge_exists("f", "c"))
    expect_true(edge_info %>% edge_exists("g", "e"))
    expect_true(edge_info %>% edge_exists("g", "f"))
})

test_that("we get the right y-coordinates", {
  tree <- make_tree()
  positions <- tree$get_node_positions()
  nodes <- tree$get_nodes()
  expect_equal(positions$label, nodes)
  expect_equal(positions$x, rep(-1, length(nodes)))
  expect_equal(positions$y, rep(-1, length(nodes)))
  tree$layout()
  positions <- tree$get_node_positions()
  expect_equal(positions$y, c(0, 0, 0, 1, 2)) # order a,b,c, d, e

  graph <- make_graph()
  positions <- graph$get_node_positions()
  nodes <- graph$get_nodes()
  expect_equal(positions$label, nodes)
  expect_equal(positions$x, rep(-1, length(nodes)))
  expect_equal(positions$y, rep(-1, length(nodes)))
  graph$layout()
  positions <- graph$get_node_positions()
  expect_equal(positions$y, c(0, 0, 0, 1, 2, 2, 3)) # order a,b,c, d e,f g

  # here we test a case that we shouldn't really see in an admixture graph,
  # but is possible to create.
  graph$add_node("h")
  graph$connect_nodes("e", "h")
  graph$layout()
  positions <- graph$get_node_positions()
  expect_equal(positions$y, c(0, 0, 0, 1, 2, 2, 3, 0)) # a,b,c, d e,f g h

  graph$add_node("i")
  graph$connect_nodes("h", "i")
  graph$layout()
  positions <- graph$get_node_positions()
  expect_equal(positions$y, c(0, 0, 0, 1, 2, 2, 3, 1, 0)) # a,b,c, d e,f g h i
})
