#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <string>
#include <map>
#include <vector>
#include <Rcpp.h>


struct Node {
    std::string name;

    std::vector<Node*> parents;
    std::vector<Node*> children;

    bool is_leaf() const { return children.empty(); }
    bool is_root() const { return parents.empty(); }

    // Used for plotting...
    double x, y;

    Node(std::string &name) : name(name) {}
    Node() : name("") {}
};

class Graph {
    std::vector<Node> nodes;
    std::map<std::string, unsigned int> nodes_map;
    void connect_nodes_(Node &parent, Node &child);

public:
    Graph() {}

    void add_node(std::string &name);
    int get_no_nodes() const { return nodes.size(); }
    Rcpp::CharacterVector get_node_names();

    void connect_nodes(std::string &parent, std::string &child);
    Rcpp::CharacterVector get_parents(std::string &node);
    Rcpp::CharacterVector get_children(std::string &node);

    Rcpp::DataFrame get_node_positions();

    bool is_connected();
};

#endif
