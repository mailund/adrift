
#' @import matchbox
dot_parse_graph <- function(text) {
    lexer <- c(
        digraph        = "digraph",
        curly_start    = "\\{",
        curly_end      = "\\}",
        square_start   = "\\[",
        square_end     = "\\]",
        stmt_end       = ";",
        arrow          = "->",
        eq             = "=",
        comma          = ",",
        text           = "\"[^\"]*\"",
        name           = "\\w+",
        whitespace     = "\\s+"
    )


    token_stream <- minilexer::lex(text, lexer)
    token_stream <- token_stream[
        !(names(token_stream) %in% c('whitespace'))
        ]
    lexer <- minilexer::TokenStream$new(token_stream)

    reset <- function(pos) {
        function(e) {
            lexer$position <- pos
            return(FALSE)
        }
    }

    parse_attr <- function() {
        pos <- lexer$position
        tryCatch({
            lexer$consume_token(type = "name")
            lexer$consume_token(type = "eq")
            lexer$consume_token(type = "text")
            lexer$consume_token(type = "stmt_end")
            return(TRUE)
        },
        error = reset(pos))
    }

    parse_attribute_list <- function() {
        tbl <- list()
        if (lexer$get_token_type() != "square_start") return(tbl)

        lexer$consume_token("square_start")
        while (lexer$get_token_type() != "square_end") {
            key <- lexer$consume_token(type = "name")
            lexer$consume_token(type = "eq")
            value <- lexer$consume_token()
            tbl[key] <- value
            if (lexer$get_token_type() == "comma") lexer$consume_token()
        }
        lexer$consume_token("square_end")
        return(tbl)
    }

    parse_node <- function() {
        pos <- lexer$position
        tryCatch({
            lexer$consume_token(type = "name")
            parse_attribute_list()
            lexer$consume_token(type = "stmt_end")
            return(TRUE)
        },
        error = reset(pos))
    }

    edges <- NIL
    parse_edge <- function() {
        pos <- lexer$position
        tryCatch({
            from <- lexer$consume_token(type = "name")
            lexer$consume_token(type = "arrow")
            to <- lexer$consume_token(type = "name")
            attribs <- parse_attribute_list()
            lexer$consume_token(type = "stmt_end")
            label <- ifelse(!is.null(attribs[["label"]]),
                            attribs[["label"]], NA)
            edge <- c(from = from, to = to, label = label)
            edges <<- CONS(edge, edges)
            return(TRUE)
        },
        error = reset(pos))
    }

    parsers <- c(parse_attr, parse_node, parse_edge)

    lexer$consume_token(type = "digraph")
    lexer$consume_token(type = "name")
    lexer$consume_token(type = "curly_start")
    while (lexer$get_token_type() != "curly_end") {
        progress <- FALSE
        for (parser in parsers) {
            progress <- parser()
            if (progress) break
        }
        if (!progress) {
            stop(paste0(
                "No parser rule matched at token position ",
                lexer$position
            ))
        }
    }
    lexer$consume_token(type = "curly_end")

    edges
}

#' Parse a graph in GraphViz' .dot format
#'
#' @param text Text containing the graph description.
#' @import matchbox
#' @export
parse_dot <- function(text) {
    graph <- new(Graph)
    edges <- dot_parse_graph(text)
    admixture_vars <- NIL
    admixture_props <- NIL
    while (!ll_is_nil(edges)) {
        edge <- edges$car
        from <- edge[1] ; to <- edge[2] ; prop <- edge[3]

        if (!graph$has_node(from)) graph$add_node(from)
        if (!graph$has_node(to)) graph$add_node(to)
        graph$connect_nodes(from, to)

        if (grepl(".*%", prop)) { # Check if we have a percentage number
            edge_name <- paste0(from, "_", to)
            admix_prop <- as.numeric(sub("%","",gsub('"','',prop))) / 100
            admixture_vars <- CONS(edge_name, admixture_vars)
            admixture_props <- CONS(admix_prop, admixture_props)
        }
        edges <- edges$cdr
    }

    if (ll_is_nil(admixture_vars)) {
        admixture_proportions <- numeric(0)
    } else {
        admixture_proportions <- as.vector(admixture_props)
        names(admixture_proportions) <- as.vector(admixture_vars)
    }
    attr(graph, "admixture_proportions") <- admixture_proportions

    graph
}
