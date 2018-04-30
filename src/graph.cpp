#include <Rcpp.h>
using namespace Rcpp;

struct Graph {
    Graph() {}
};


RCPP_MODULE(Graph) {
    class_<Graph>("Graph")
    .constructor()
    ;
}
