
#' Construct a new graph object
#'
#' @return A new admixture/drift graph object
#' @export
new_graph <- function() {
    structure(new_graph_(), class = "adrift_graph")
}

#' @export
print.adrift_graph <- function(x, ...) {
    cat("Admixture graph object\n")
}
