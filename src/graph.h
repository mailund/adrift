#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <string>
#include <map>
#include <vector>
#include <Rcpp.h>


struct Node {
    std::string name;
    std::vector<unsigned int> parents;
    std::vector<unsigned int> children;

    // Used for plotting...
    double x;
    int dist_to_leaf;

public:
    Node(std::string &name) : name(name), x(-1.0), dist_to_leaf(-1) {}
    Node() : name(""), x(-1.0), dist_to_leaf(-1) {}

    const std::string &get_name() const { return name; }

    double get_x() const { return x; }
    double get_y() const { return (double)dist_to_leaf; }
    void set_x(double new_x) { x = new_x; }

    bool is_leaf() const { return children.empty(); }
    bool is_root() const { return parents.empty(); }


};

class Graph {
    std::vector<Node> nodes;
    std::map<std::string, unsigned int> nodes_map;
    std::vector< std::pair<unsigned int, unsigned int> > edges;

    void connect_nodes_(unsigned int parent_idx, unsigned int child_idx);

    void assign_initial_coordinates();
    void compute_forces(std::vector<double> &x, double drag);
    void force_step(double drag);


    void compute_dist_to_leaf(Node &node);
    double assign_x_coordinate(Node &node, int &node_no);

public:
    Graph() {}

    void add_node(std::string &name);
    int get_no_nodes() const { return nodes.size(); }
    Rcpp::CharacterVector get_node_names();

    Rcpp::LogicalVector is_leaf();

    void connect_nodes(std::string &parent, std::string &child);
    Rcpp::CharacterVector get_parents(std::string &node);
    Rcpp::CharacterVector get_children(std::string &node);

    void graph_layout();
    Rcpp::DataFrame get_node_positions();

    Rcpp::DataFrame get_ggraph_nodes();
    Rcpp::DataFrame get_ggraph_edges();

    bool is_connected();
};

#endif
