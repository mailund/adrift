#' @useDynLib adrift, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Rcpp module: Graph
#' @name Graph
#' @export
NULL
Rcpp::loadModule("Graph", TRUE)
