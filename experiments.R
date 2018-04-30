
g <- new(Graph)
g$add_node("x") ; g$add_node("y") ; g$add_node("z")
g$nodes

g$connect_nodes("x", "y")
g$connect_nodes("x", "z")
g$get_children("x")
g$get_parents("y")
g$get_parents("z")

g$connected

