#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <string>
#include <map>
#include <vector>
#include <Rcpp.h>


class Node {
    friend class Graph;

    std::string name;

    std::vector<Node*> parents;
    std::vector<Node*> children;

    // Used for plotting...
    double x;
    int dist_to_leaf;

public:
    Node(std::string &name) : name(name), dist_to_leaf(-1) {}
    Node() : name(""), dist_to_leaf(-1) {}

    const std::string &get_name() const { return name; }

    double get_x() const { return x; }
    double get_y() const { return (double)dist_to_leaf; }
    void set_x(double new_x) { x = new_x; }

    bool is_leaf() const { return children.empty(); }
    bool is_root() const { return parents.empty(); }

    void compute_dist_to_leaf();

};

class Graph {
    std::vector<Node> nodes;
    std::map<std::string, unsigned int> nodes_map;
    void connect_nodes_(Node &parent, Node &child);

public: // public now, but once layout is done, make it private
    void randomize_node_positions();
    void compute_forces(std::vector<double> &x, double drag);
    void force_step(double drag);

public:
    Graph() {}

    void add_node(std::string &name);
    int get_no_nodes() const { return nodes.size(); }
    Rcpp::CharacterVector get_node_names();

    void connect_nodes(std::string &parent, std::string &child);
    Rcpp::CharacterVector get_parents(std::string &node);
    Rcpp::CharacterVector get_children(std::string &node);

    Rcpp::DataFrame get_node_positions();

    Rcpp::DataFrame get_ggraph_nodes();
    Rcpp::DataFrame get_ggraph_edges();

    bool is_connected();
};

#endif
