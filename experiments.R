
g <- new(Graph)
g$add_node("x") ; g$add_node("y")
g$connect_nodes("x", "y")
g$get_parents("y")
g$get_children("x")
