#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <string>
#include <map>
#include <vector>
#include <Rcpp.h>

struct Node {
    std::string name;
    Rcpp::CharacterVector parents;
    Rcpp::CharacterVector children;

    Node(std::string &name) : name(name) {}
    Node() : name("") {}
};

class Graph {
    std::map<std::string, Node> nodes;
    void connect_nodes_(Node &parent, Node &child);

public:
    Graph() {}

    void add_node(std::string &name);
    int get_no_nodes() const { return nodes.size(); }

    void connect_nodes(std::string &parent, std::string &child);
    Rcpp::CharacterVector get_parents(std::string &node);
    Rcpp::CharacterVector get_children(std::string &node);
};

#endif
