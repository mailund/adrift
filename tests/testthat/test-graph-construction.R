context("Graph Construction")

test_that("we can construct a simple graph", {
  g <- new(Graph)
  g$add_node("a") ; g$add_node("b")
  expect_equal(g$nodes, c("a", "b"))
  expect_false(g$connected)

  g$connect_nodes("a", "b")
  expect_true(g$connected)

  expect_equal(g$get_children("a"), "b")
  expect_equal(g$get_parents("b"), "a")
  skip("we cannot yet access is_leaf vector by name")
  #expect_true(g$is_leaf["b"])
  #expect_false(g$is_leaf["a"])
  expect_equal(g$is_leaf, c(FALSE, TRUE))
})
