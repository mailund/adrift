#include "graph.h"
#include <stack>

// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;


void Node::compute_dist_to_leaf()
{
    if (dist_to_leaf >= 0) return;

    dist_to_leaf = 0;
    for (auto child : children) {
        child->compute_dist_to_leaf();
        dist_to_leaf = std::max(dist_to_leaf, child->dist_to_leaf + 1);
    }
}

double Node::assign_x_coordinate(int &node_no)
{
    if (x >= 0) return x;

    if (is_leaf()) {
        return x = (double)node_no++;
    } else {
        if (children.size() == 1) {
            return x = children[0]->assign_x_coordinate(node_no);

        } else if (children.size() == 2){
            RNGScope rng;
            double sum_x = 0.0;
            bool order = runif(1, 0, 1)[0] < 0.5;
            sum_x += children[order]->assign_x_coordinate(node_no);
            sum_x += children[!order]->assign_x_coordinate(node_no);
            return x = sum_x / 2.0;

        } else {
            // There shouldn't be high degree nodes like this, but
            // just in case, we do this
            double sum_x = 0.0;
            for (auto child : children) {
                sum_x += child->assign_x_coordinate(node_no);
            }
            return x = sum_x / children.size();
        }
    }
}

void Graph::add_node(std::string &name)
{
    if (nodes_map.find(name) != nodes_map.end()) {
        warning("Node already in graph!");
        return;
    }
    nodes.push_back(Node(name));
    nodes_map[name] = nodes.size() - 1;
}

CharacterVector Graph::get_node_names()
{
    CharacterVector names(nodes.size());
    for (int i = 0; i < nodes.size(); ++i) {
        names[i] = nodes[i].get_name();
    }
    return names;
}

LogicalVector Graph::is_leaf()
{
    LogicalVector is_leaf_v(nodes.size());
    for (int i = 0; i < nodes.size(); ++i) {
        is_leaf_v[i] = nodes[i].is_leaf();
    }
    return is_leaf_v;
}

void Graph::connect_nodes_(Node &parent, Node &child)
{
    parent.children.push_back(&child);
    child.parents.push_back(&parent);
}

void Graph::connect_nodes(std::string &parent, std::string &child)
{
    if (nodes_map.find(parent) == nodes_map.end()) {
        stop("The parent node is not found in the graph.");
        return;
    }
    if (nodes_map.find(child) == nodes_map.end()) {
        stop("The child node is not found in the graph.");
        return;
    }

    unsigned int parent_idx = nodes_map[parent];
    unsigned int child_idx = nodes_map[child];
    Node &parent_node = nodes[parent_idx];
    Node &child_node = nodes[child_idx];
    connect_nodes_(parent_node, child_node);
    edges.push_back(std::pair<unsigned int, unsigned int>(parent_idx, child_idx));
}

CharacterVector Graph::get_parents(std::string &node_name)
{
    if (nodes_map.find(node_name) == nodes_map.end()) {
        warning("Node is not found in graph.");
        return R_NilValue;
    }
    Node &node = nodes[nodes_map[node_name]];
    CharacterVector parent_names(node.parents.size());
    for (int i = 0; i < node.parents.size(); ++i) {
        parent_names.push_back(node.parents[i]->name);
    }
    return parent_names;
}

CharacterVector Graph::get_children(std::string &node_name)
{
    if (nodes_map.find(node_name) == nodes_map.end()) {
        warning("Node is not found in graph!");
        return R_NilValue;
    }
    Node &node = nodes[nodes_map[node_name]];
    CharacterVector children_names(node.children.size());
    for (int i = 0; i < node.children.size(); ++i) {
        children_names.push_back(node.children[i]->name);
    }
    return children_names;
}

bool Graph::is_connected()
{
    if (nodes.empty()) {
        warning("Graph is empty!");
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

    return seen.size() == nodes.size();
}

void Graph::assign_initial_coordinates()
{
    int node_no = 0;
    for (auto &n : nodes) {
        n.assign_x_coordinate(node_no); // assign x-coord. tree-like
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

            // FIXME: better to compute levels first than go over all pairs https://github.com/mailund/adrift/issues/10 id:8
            if (a.get_y() != b.get_y()) continue;

            // v - vector from b to a
            double vx = a.get_x() - b.get_x();
            if (vx == 0.0) vx = 1e-5; // avoid having them on top of each other
            double dist_sq = vx*vx;
            // force acting on a
            double fa_x = drag / dist_sq * (vx / ::sqrt(dist_sq));
            // force acting on b
            double fb_x = - fa_x;

            // update forces
            x[i] += fa_x / 5.0;
            x[j] += fb_x / 5.0;
        }
    }

    // for the spring forces, I compute all parent-child edges but only move
    // children (parent moved by gravety)
    for (int i = 0; i < n; ++i) {
        Node &a = nodes[i];
        for (auto bp : a.children) {
            Node &b = *bp;
            int j = nodes_map[b.name];

            double vx = a.get_x() - b.get_x();
            if (vx == 0.0) vx = 1e-5; // avoid having them on top of each other

            // force acting on b
            double fa_x = - drag * vx;
            double fb_x = - fa_x;

            // update forces
            x[i] += fa_x  / 5.0;
            x[j] += fb_x  / 5.0;
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

void Graph::graph_layout()
{
    if (!is_connected()) {
        stop("The graph needs to be connected before the "
             "layout algorithm can be used.");
        return;
    }

    assign_initial_coordinates();
    return;
    for (int i = 1; i <= 100; ++i) {
        double drag = 1.0 / i;
        for (int j = 1; j <= 100; ++j) {
            force_step(drag);
        }
    }
}

DataFrame Graph::get_node_positions()
{
    CharacterVector label(nodes.size());
    NumericVector x(nodes.size()), y(nodes.size());
    for (int i = 0; i < nodes.size(); ++i) {
        Node &n = nodes[i];
        label.push_back(n.name);
        x.push_back(n.get_x());
        y.push_back(n.get_y());
    }
    return DataFrame::create(Named("label") = label,
                             Named("x") = x, Named("y") = y);
}

DataFrame Graph::get_ggraph_nodes()
{
    CharacterVector node_names(get_node_names());
    return DataFrame::create(Named("label") = node_names,
                             Named("is_leaf") = is_leaf());
}

DataFrame Graph::get_ggraph_edges()
{
    CharacterVector from(edges.size()), to(edges.size());
    for (int i = 0; i < edges.size(); ++i) {
        std::pair<unsigned int,unsigned int> edge = edges[i];
        Node &parent = nodes[edge.first];
        Node &child = nodes[edge.second];
        from[i] = parent.name;
        to[i] = child.name;
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
        .property("is_leaf", &Graph::is_leaf)

        .method("connect_nodes", &Graph::connect_nodes)

        .method("get_parents", &Graph::get_parents)
        .method("get_children", &Graph::get_children)

        .property("connected", &Graph::is_connected)
        .method("layout", &Graph::graph_layout)
        .property("node_positions", &Graph::get_node_positions)

        .property("ggraph_nodes", &Graph::get_ggraph_nodes)
        .property("ggraph_edges", &Graph::get_ggraph_edges)
    ;
}
