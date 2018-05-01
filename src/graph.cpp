#include "graph.hpp"
#include <stack>

using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

void Node::compute_dist_to_leaf()
{
    if (dist_to_leaf >= 0) return;

    dist_to_leaf = 0;
    for (auto child : children) {
        child->compute_dist_to_leaf();
        dist_to_leaf = std::max(dist_to_leaf, child->dist_to_leaf + 1);
    }
}

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
        names.push_back(cell.get_name());
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

void Graph::randomize_node_positions()
{
    for (auto &n : nodes) {
        // somewhat arbitrary sampling...
        n.set_x(rnorm(1, 0, 1)[0]);
        n.compute_dist_to_leaf(); // y coordinate determined by level
    }
}

void Graph::compute_forces(std::vector<double> &x, double drag)
{
    int n = get_no_nodes();

    // for the repulsive forces I compute all i,j : j < i pairs
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            Node &a = nodes[i];
            Node &b = nodes[j];

            // fixme: better to compute levels first than go over all pairs
            if (a.get_y() != b.get_y()) continue;

            // v - vector from b to a
            double vx = a.get_x() - b.get_x();
            double dist_sq = vx*vx;
            // force acting on a
            double fa_x = drag / dist_sq * (vx / ::sqrt(dist_sq));
            // force acting on b
            double fb_x = - fa_x;

            // update forces
            x[i] += fa_x;
            x[j] += fb_x;
        }
    }

    // for the spring forces, I compute all parent-child edges but only move children (parent moved by gravety)
    for (int i = 0; i < n; ++i) {
        Node &a = nodes[i];
        for (auto bp : a.children) {
            Node &b = *bp;
            int j = nodes_map[b.name];

            double vx = a.get_x() - b.get_x();
            // force acting on b
            double fb_x = - drag * vx;

            // update forces
            x[j] += fb_x  / 10.0; // 10.0 to make it a weaker force
        }
    }

    // for the gravety force I move parents to the center of children's gravety
    for (int i = 0; i < n; ++i) {
        Node &a = nodes[i];
        if (a.children.empty()) continue;

        double sum_x = 0.0;
        for (auto bp : a.children) {
            sum_x += bp->get_x();
        }
        double mean_x = sum_x / a.children.size();
        double force = - drag * (a.get_x() - mean_x);
        x[i] += force ; // / 10.0; // 10.0 to make it a weaker force
    }

    for (int i = 0; i < n; ++i) {
        Node &a = nodes[i];
        std::cout << "node " << a.name << " will be adjusted by delta x = " << x[i] << std::endl;
    }

}

void Graph::force_step(double drag)
{
    int n = get_no_nodes();
    std::vector<double> force_x(n, 0.0);
    compute_forces(force_x, drag);
    for (int i = 0; i < n; ++i) {
        Node &node = nodes[i];
        node.x += force_x[i] / 10.0; // 10.0 to control rate of change
    }
}

DataFrame Graph::get_node_positions()
{
    CharacterVector label;
    NumericVector x, y;
    for (auto n : nodes) {
        label.push_back(n.name);
        x.push_back(n.get_x());
        y.push_back(n.get_y());
    }
    return DataFrame::create(Named("label") = label,
                             Named("x") = x, Named("y") = y);
}

DataFrame Graph::get_ggraph_nodes()
{
    return DataFrame::create(Named("label") = get_node_names());
}

DataFrame Graph::get_ggraph_edges()
{
    CharacterVector from, to;
    for (auto &n : nodes) {
        for (auto child : n.children) {
            from.push_back(n.name);
            to.push_back(child->name);
        }
    }
    return DataFrame::create(Named("from") = from,
                             Named("to") = to,
                             Named("stringsAsFactors") = false);
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

        // remove these from public interface once layout algorithm is done
        .method("randomize_node_positions", &Graph::randomize_node_positions)
        .method("force_step", &Graph::force_step)

        .property("node_positions", &Graph::get_node_positions)

        .property("ggraph_nodes", &Graph::get_ggraph_nodes)
        .property("ggraph_edges", &Graph::get_ggraph_edges)
    ;
}
