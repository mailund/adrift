context("Graph Construction")

test_that("we can construct a simple graph", {
  g <- new(Graph)
  g$add_node("a") ; g$add_node("b")
  expect_equal(g$nodes, c("a", "b"))
  expect_false(g$connected)

  g$connect_nodes("a", "b")
  expect_true(g$connected)
})
