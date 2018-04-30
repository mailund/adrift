
#' Read a list of edges from a 'dot' description.
#'
#' @param text A string that contains a graph in the `dot` format from
#'             GraphViz.
#' @return A linked list containing all the edges in the GraphViz graph.
#'         Each edge has an associated label. We can use this to import
#'         admixture proportions estimated by qpGraph.
#'
#' @export
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
        lexer$consume_token("square_start")
        tbl <- list()
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

    edges <- matchbox::NIL
    parse_edge <- function() {
        pos <- lexer$position
        tryCatch({
            from <- lexer$consume_token(type = "name")
            lexer$consume_token(type = "arrow")
            to <- lexer$consume_token(type = "name")
            attribs <- parse_attribute_list()
            lexer$consume_token(type = "stmt_end")
            edge <- c(from = from, to = to, label = attribs[["label"]])
            edges <<- matchbox::CONS(edge, edges)
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
            stop(paste0("No parser rule matched at token position ", lexer$position))
        }
    }
    lexer$consume_token(type = "curly_end")

    edges
}


dot_get_edges <- function(edges) {
    no_edges <- matchbox::llength(edges)
    tbl <- character(length = no_edges * 3)
    dim(tbl) <- c(no_edges, 3)

    idx <- 1 ; e <- edges
    while (!matchbox::ll_is_nil(e)) {
        tbl[idx,] <- e$car
        e <- e$cdr
        idx <- idx + 1
    }

    colnames(tbl) <- c("from", "to", "label")
    tbl
}

