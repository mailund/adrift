#include "graph.hpp"
#include <stack>

using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

void Graph::add_node(std::string &name)
{
    if (nodes.find(name) != nodes.end()) {
        // FIXME
        std::cerr << "Node already in graph!\n";
    }
    nodes[name] = Node(name);
}

CharacterVector Graph::get_node_names()
{
    CharacterVector names;
    for (auto cell : nodes) {
        names.push_back(cell.second.name);
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
    CharacterVector parent_names;
    for (auto n : node.parents) {
        parent_names.push_back(n->name);
    }
    return parent_names;
}

Rcpp::CharacterVector Graph::get_children(std::string &node_name)
{
    Node &node = nodes[node_name]; // FIXME: error handling
    CharacterVector children_names;
    for (auto n : node.children) {
        children_names.push_back(n->name);
    }
    return children_names;
}

bool Graph::is_connected()
{
    std::set<Node*> seen;
    std::stack<Node*> to_process;
    Node *node = &(nodes.begin()->second);
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

RCPP_MODULE(Graph) {
    class_<Graph>("Graph")
        .constructor()

        .method("add_node", &Graph::add_node)
        .property("no_nodes", &Graph::get_no_nodes, 0)
        .property("nodes", &Graph::get_node_names, 0)

        .method("connect_nodes", &Graph::connect_nodes)

        .method("get_parents", &Graph::get_parents)
        .method("get_children", &Graph::get_children)

        .property("connected", &Graph::is_connected)
    ;
}
