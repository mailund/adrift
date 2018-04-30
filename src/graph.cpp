#include "graph.hpp"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP new_graph_() {
    Rcpp::XPtr<Graph> ptr( new Graph(), true );
    return ptr;
}

// [[Rcpp::export]]
SEXP add_node(SEXP graph, CharacterVector name) {

}
