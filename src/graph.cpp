#include "graph.hpp"
#include <stack>

using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

void Graph::add_node(std::string &name)
{
    if (nodes_map.find(name) != nodes_map.end()) {
        // FIXME: make warning
        std::cerr << "Node already in graph!\n";
        return;
    }
    nodes.push_back(Node(name));
    nodes_map[name] = nodes.size() - 1;
}

CharacterVector Graph::get_node_names()
{
    CharacterVector names;
    for (auto cell : nodes) {
        names.push_back(cell.name);
    }
    return names;
}

void Graph::connect_nodes_(Node &parent, Node &child)
{
    // FIXME: data validation
    parent.children.push_back(&child);
    child.parents.push_back(&parent);
}

void Graph::connect_nodes(std::string &parent, std::string &child)
{
    // FIXME: better error handling
    if (nodes_map.find(parent) == nodes_map.end()) {
        std::cerr << "The parent node " << parent << "is not found in the graph!\n";
        return;
    }
    if (nodes_map.find(child) == nodes_map.end()) {
        std::cerr << "The child node " << child << "is not found in the graph!\n";
        return;
    }

    Node &parent_node = nodes[nodes_map[parent]];
    Node &child_node = nodes[nodes_map[child]];
    connect_nodes_(parent_node, child_node);
}

CharacterVector Graph::get_parents(std::string &node_name)
{
    if (nodes_map.find(node_name) == nodes_map.end()) {
        std::cerr << "Node is not found in graph!\n";
        return R_NilValue;
    }
    Node &node = nodes[nodes_map[node_name]];
    CharacterVector parent_names;
    for (auto n : node.parents) {
        parent_names.push_back(n->name);
    }
    return parent_names;
}

CharacterVector Graph::get_children(std::string &node_name)
{
    if (nodes_map.find(node_name) == nodes_map.end()) {
        std::cerr << "Node is not found in graph!\n";
        return R_NilValue;
    }
    Node &node = nodes[nodes_map[node_name]];
    CharacterVector children_names;
    for (auto n : node.children) {
        children_names.push_back(n->name);
    }
    return children_names;
}

bool Graph::is_connected()
{
    if (nodes.empty()) {
        std::cerr << "Graph is empty!\n"; // FIXME: make this a warning
        return true;
    }

    std::set<Node*> seen;
    std::stack<Node*> to_process;
    Node *node = &nodes[0];
    to_process.push(node); seen.insert(node);

    while (!to_process.empty()) {
        node = to_process.top(); to_process.pop();
        for (auto n : node->parents) {
            if (seen.find(n) == seen.end()) {
                to_process.push(n);
                seen.insert(n);
            }
        }
        for (auto n : node->children) {
            if (seen.find(n) == seen.end()) {
                to_process.push(n);
                seen.insert(n);
            }
        }
    }
    std::cout << seen.size() << std::endl;
    return seen.size() == nodes.size();
}

DataFrame Graph::get_node_positions()
{
    CharacterVector label;
    NumericVector x, y;
    for (auto n : nodes) {
        label.push_back(n.name);
        x.push_back(n.x);
        y.push_back(n.y);
    }
    return DataFrame::create(Named("label") = label,
                             Named("x") = x, Named("y") = y);
}

RCPP_MODULE(Graph) {
    class_<Graph>("Graph")
        .constructor()

        .method("add_node", &Graph::add_node)
        .property("no_nodes", &Graph::get_no_nodes)
        .property("nodes", &Graph::get_node_names)

        .method("connect_nodes", &Graph::connect_nodes)

        .method("get_parents", &Graph::get_parents)
        .method("get_children", &Graph::get_children)

        .property("connected", &Graph::is_connected)

        .property("node_positions", &Graph::get_node_positions)
    ;
}
