context("Graph Construction")

test_that("we can construct a simple graph", {
  g <- new(Graph)
  expect_true(expect_warning(g$connected, "Graph is empty!"))

  g$add_node("a") ; g$add_node("b")
  expect_equal(g$no_nodes, 2)
  expect_equal(g$nodes, c("a", "b"))
  expect_false(g$connected)

  expect_warning(g$add_node("a"), "Node already in graph!")

  g$connect_nodes("a", "b")
  expect_equal(g$no_nodes, 2)
  expect_true(g$connected)

  expect_error(g$connect_nodes("foo", "b"),
               "The parent node is not found in the graph.")
  expect_error(g$connect_nodes("a", "foo"),
               "The child node is not found in the graph.")

  expect_equal(g$get_children("a"), "b")
  expect_equal(g$get_parents("b"), "a")

  expect_equal(expect_warning(g$get_parents("foo"), "Node is not found.*"),
               character(0))
  expect_equal(expect_warning(g$get_children("foo"), "Node is not found.*"),
               character(0))

  expect_equal(g$is_leaf, c(a = FALSE, b = TRUE))
  expect_true(g$is_leaf["b"])
  expect_false(g$is_leaf["a"])
})
