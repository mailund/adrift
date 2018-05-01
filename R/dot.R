
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

    edges <- NIL
    parse_edge <- function() {
        pos <- lexer$position
        tryCatch({
            from <- lexer$consume_token(type = "name")
            lexer$consume_token(type = "arrow")
            to <- lexer$consume_token(type = "name")
            attribs <- parse_attribute_list()
            lexer$consume_token(type = "stmt_end")
            edge <- c(from = from, to = to, label = attribs[["label"]])
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
            stop(paste0("No parser rule matched at token position ", lexer$position))
        }
    }
    lexer$consume_token(type = "curly_end")

    edges
}

#' @import matchbox
dot_get_edges <- function(edges) {
    no_edges <- llength(edges)
    tbl <- character(length = no_edges * 3)
    dim(tbl) <- c(no_edges, 3)

    idx <- 1 ; e <- edges
    while (!ll_is_nil(e)) {
        tbl[idx,] <- e$car
        e <- e$cdr
        idx <- idx + 1
    }

    colnames(tbl) <- c("from", "to", "label")
    tbl
}


#' Import a dot file into an admixturegraph object
#'
#' @param text Text containing the graph description.
#' @return An admixturegraph object
#' @import admixturegraph
#' @import dplyr
#' @import matchbox
#' @import tibble
#' @export
read_dot <- function(text) {
    edges <- dot_parse_graph(text)
    return(edges)

    edges_tbl <- dot_get_edges(edges)
    edges_df <- as_tibble(edges_tbl)
    return(edges_df)

    count_in <- edges_df %>%
        group_by(to) %>%
        mutate(in_degree = n()) %>%
        ungroup() %>%
        select(to, in_degree) %>%
        rename(node = to)

    count_out <- edges_df %>%
        group_by(from) %>%
        mutate(out_degree = n()) %>%
        ungroup() %>%
        select(from, out_degree) %>%
        rename(node = from)

    degrees <- full_join(count_in, count_out, by = "node") %>%
        mutate(in_degree = ifelse(is.na(in_degree), 0, in_degree),
               out_degree = ifelse(is.na(out_degree), 0, out_degree))

    leaves <- degrees %>%
        filter(out_degree == 0) %>%
        select(node) %>% .[[1]] %>% unique()

    inner_nodes <- degrees %>%
        filter(out_degree > 0) %>%
        select(node) %>% .[[1]] %>% unique()

    admixture_nodes <- degrees %>%
        filter(in_degree == 2) %>%
        select(node) %>% .[[1]] %>% unique()

    edges_tbl[,1:3] <- edges_tbl[,c(2,1,3)]
    edges_tbl[,3] <- NA
    colnames(edges_tbl) <- c("child", "parent", "prop")

    admixture_vars <- NIL
    admixture_props <- NIL
    for (node in admixture_nodes) {
        edge <- edges_df %>% filter(to == node) %>% head(1)
        edge_name <- paste0(edge$from, "_", edge$to)
        edge_prop <- as.numeric(sub("%","",gsub('"','',edge$label))) / 100
        edges_tbl[edges_tbl[,"parent"] == edge$from & edges_tbl[,"child"] == edge$to,3] <- edge_name
        admixture_vars <- CONS(edge_name, admixture_vars)
        admixture_props <- CONS(edge_prop, admixture_props)
    }
    admixture_vars <- admixture_vars %>% as.vector()
    admixture_props <- admixture_props %>% as.vector()
    if (length(admixture_props) > 0)
        names(admixture_props) <- admixture_vars

    g <- agraph(leaves, inner_nodes, edges_tbl)
    attr(g, "admixture_proportions") <- admixture_props
    g
}


