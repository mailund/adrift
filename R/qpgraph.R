
#' Parse a qpGraph text.
#' @param text The text description of the graph
#' @return An admixture graph
#' @export
parse_qpgraph <- function(text) {
    tokens <- c(
        root        = "root",
        label       = "label",
        edge        = "edge",
        admix       = "admix",
        number      = "-?\\d*\\.?\\d+",
        name        = "\\w+",
        newline     = "\\n",
        whitespace  = "\\s+"
    )


    token_stream <- minilexer::lex(text, tokens)
    token_stream <- token_stream[
        !(names(token_stream) %in% c('whitespace', 'newline'))
        ]
    lexer <- minilexer::TokenStream$new(token_stream)

    edges <- matchbox::NIL
    admixtures <- matchbox::NIL

    parse_root <- function() {
        lexer$consume_token(type = "root")
        lexer$consume_token(type = "name")
    }

    parse_label <- function() {
        lexer$consume_token(type = "label")
        lexer$consume_token(type = "name")
        lexer$consume_token(type = "name")
    }

    parse_edge <- function() {
        lexer$consume_token(type = "edge")
        edge_name <- lexer$consume_token(type = "name")
        parent <- lexer$consume_token(type = "name")
        child <- lexer$consume_token(type = "name")
        edge <- c(child = child, parent = parent, NA)
        edges <<- matchbox::CONS(edge, edges)
    }

    parse_admixture <- function() {
        lexer$consume_token(type = "admix")
        admixed <- lexer$consume_token(type = "name")
        parent_1 <- lexer$consume_token(type = "name")
        parent_2 <- lexer$consume_token(type = "name")
        alpha <- lexer$consume_token(type = "number")
        lexer$consume_token(type = "number")
        admixture <- c(
            admixed = admixed,
            parent_1 = parent_1,
            parent_2 = parent_2,
            alpha = as.numeric(alpha) / 100 # from percentages to fractions
        )
        admixtures <<- matchbox::CONS(admixture, admixtures)
    }

    while (!lexer$end_of_stream) {
        switch(lexer$current_type(),
               "root" = parse_root(),
               "label" = parse_label(),
               "edge" = parse_edge(),
               "admix" = parse_admixture()
        )
    }

    graph <- methods::new(Graph)
    while (!matchbox::ll_is_nil(edges)) {
        e <- edges$car
        parent <- e["parent"]
        child <- e["child"]

        if (!graph$has_node(parent)) graph$add_node(parent)
        if (!graph$has_node(child))  graph$add_node(child)
        graph$connect_nodes(parent, child)

        edges <- edges$cdr
    }
    admixture_vars <- matchbox::NIL
    admixture_props <- matchbox::NIL
    while (!matchbox::ll_is_nil(admixtures)) {
        a <- admixtures$car

        parent1 <- a["parent_1"]
        parent2 <- a["parent_2"]
        child <- a["admixed"]
        alpha <- as.numeric(a["alpha"])

        if (!graph$has_node(parent1)) graph$add_node(parent1)
        if (!graph$has_node(parent2)) graph$add_node(parent2)
        if (!graph$has_node(child))   graph$add_node(child)
        graph$connect_nodes(parent1, child)
        graph$connect_nodes(parent2, child)

        admixture_var <- paste0(parent1, "_", child)
        admixture_vars <- matchbox::CONS(admixture_var, admixture_vars)
        admixture_props <- matchbox::CONS(alpha, admixture_props)

        admixtures <- admixtures$cdr
    }
    admixture_vars <- as.vector(admixture_vars)
    admixture_props <- as.vector(admixture_props)
    if (length(admixture_props) > 0)
        names(admixture_props) <- admixture_vars
    attr(graph, "admixture_proportions") <- admixture_props
    graph

}
