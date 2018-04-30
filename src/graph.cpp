#include "graph.hpp"

using namespace Rcpp;

void Graph::add_node(std::string &name)
{
    if (nodes.find(name) != nodes.end()) {
        // FIXME
        std::cerr << "Node already in graph!\n";
    }
    nodes[name] = Node(name);
}

void Graph::connect_nodes_(Node &parent, Node &child)
{
    // FIXME: data validation
    parent.children.push_back(child.name);
    child.parents.push_back(parent.name);
}

void Graph::connect_nodes(std::string &parent, std::string &child)
{
    // FIXME: better error handling
    if (nodes.find(parent) == nodes.end()) {
        std::cerr << "The parent node " << parent << "is not found in the graph!\n";
        return;
    }
    if (nodes.find(child) == nodes.end()) {
        std::cerr << "The child node " << child << "is not found in the graph!\n";
        return;
    }

    Node &parent_node = nodes[parent];
    Node &child_node = nodes[child];
    connect_nodes_(parent_node, child_node);
}

Rcpp::CharacterVector Graph::get_parents(std::string &node_name)
{
    Node &node = nodes[node_name]; // FIXME: error handling
    return node.parents;
}

Rcpp::CharacterVector Graph::get_children(std::string &node_name)
{
    Node &node = nodes[node_name]; // FIXME: error handling
    return node.children;
}

RCPP_MODULE(Graph) {
    class_<Graph>("Graph")
        .constructor()

        .method("add_node", &Graph::add_node)
        .property("no_nodes", &Graph::get_no_nodes, 0)

        .method("connect_nodes", &Graph::connect_nodes)

        .method("get_parents", &Graph::get_parents)
        .method("get_children", &Graph::get_children)
    ;



}
