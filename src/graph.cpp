#include "graph.h"
#include <stack>

// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

void Graph::reset_coordinates()
{
    for (Node &node : nodes) {
        node.reset();
    }
}

void Graph::compute_dist_to_leaf(Node &node)
{
    if (node.dist_to_leaf >= 0) return;

    node.dist_to_leaf = 0;
    for (unsigned int child_idx : node.children) {
        Node &child = nodes[child_idx];
        compute_dist_to_leaf(child);
        node.dist_to_leaf = std::max(node.dist_to_leaf, child.dist_to_leaf + 1);
    }
}

double Graph::assign_x_coordinate(Node &node, int &node_no)
{
    if (node.x >= 0) return node.x;

    if (node.is_leaf()) {
        return node.x = (double)node_no++;
    } else {
        if (node.children.size() == 1) {
            Node &child = nodes[node.children[0]];
            return node.x = assign_x_coordinate(child, node_no);

        } else if (node.children.size() == 2){
            RNGScope rng;
            double sum_x = 0.0;
            bool order = runif(1, 0, 1)[0] < 0.5;
            Node &left = nodes[node.children[order]];
            Node &right = nodes[node.children[!order]];
            sum_x += assign_x_coordinate(left, node_no);
            sum_x += assign_x_coordinate(right, node_no);
            return node.x = sum_x / 2.0;

        } else {
            // There shouldn't be high degree nodes like this, but
            // just in case, we do this
            double sum_x = 0.0;
            for (unsigned int child_idx : node.children) {
                Node &child = nodes[child_idx];
                sum_x += assign_x_coordinate(child, node_no);
            }
            return node.x = sum_x / node.children.size();
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

CharacterVector Graph::get_node_names() const
{
    CharacterVector names(nodes.size());
    for (int i = 0; i < nodes.size(); ++i) {
        names[i] = nodes[i].get_name();
    }
    return names;
}

bool Graph::has_node(std::string &name) const
{
    return nodes_map.find(name) != nodes_map.end();
}

LogicalVector Graph::get_leaf_status() const
{
    LogicalVector is_leaf_v(nodes.size());
    for (int i = 0; i < nodes.size(); ++i) {
        is_leaf_v[i] = nodes[i].is_leaf();
    }
    is_leaf_v.attr("names") = get_node_names();
    return is_leaf_v;
}

void Graph::connect_nodes_(unsigned int parent_idx, unsigned int child_idx)
{
    Node &parent = nodes[parent_idx];
    Node &child = nodes[child_idx];
    parent.children.push_back(child_idx);
    child.parents.push_back(parent_idx);
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
    connect_nodes_(parent_idx, child_idx);
    edges.push_back(std::pair<unsigned int, unsigned int>(parent_idx, child_idx));
}

CharacterVector Graph::get_parents(std::string &node_name) const
{
    if (nodes_map.find(node_name) == nodes_map.end()) {
        warning("Node is not found in graph.");
        return CharacterVector(0);
    }
    unsigned int node_idx = nodes_map.at(node_name);
    const Node &node = nodes[node_idx];
    CharacterVector parent_names(node.parents.size());
    for (int i = 0; i < node.parents.size(); ++i) {
        parent_names[i] = nodes[node.parents[i]].name;
    }
    return parent_names;
}

CharacterVector Graph::get_children(std::string &node_name) const
{
    if (nodes_map.find(node_name) == nodes_map.end()) {
        warning("Node is not found in graph!");
        return CharacterVector(0);
    }
    unsigned int node_idx = nodes_map.at(node_name);
    const Node &node = nodes[node_idx];
    CharacterVector children_names(node.children.size());
    for (int i = 0; i < node.children.size(); ++i) {
        children_names[i] = nodes[node.children[i]].name;
    }
    return children_names;
}

bool Graph::is_connected() const
{
    if (nodes.empty()) {
        warning("Graph is empty!");
        return true;
    }

    std::set<unsigned int> seen;
    std::stack<unsigned int> to_process;
    to_process.push(0); seen.insert(0);

    while (!to_process.empty()) {
        unsigned int node_idx = to_process.top(); to_process.pop();
        const Node &node = nodes[node_idx];
        for (unsigned int parent_idx : node.parents) {
           if (seen.find(parent_idx) == seen.end()) {
                to_process.push(parent_idx);
                seen.insert(parent_idx);
            }
        }
        for (unsigned int child_idx : node.children) {
            if (seen.find(child_idx) == seen.end()) {
                to_process.push(child_idx);
                seen.insert(child_idx);
            }
        }
    }

    return seen.size() == nodes.size();
}

void Graph::assign_initial_coordinates()
{
    int node_no = 0;
    for (auto &n : nodes) {
        assign_x_coordinate(n, node_no); // assign x-coord. tree-like
        compute_dist_to_leaf(n); // y coordinate determined by level
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

            // FIXME: better to compute levels first than go over all pairs id:8
            // https://github.com/mailund/adrift/issues/10
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
        for (unsigned int b_idx : a.children) {
            Node &b = nodes[b_idx];
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
        for (unsigned int b_idx : a.children) {
            Node &b = nodes[b_idx];
            sum_x += b.get_x();
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

    reset_coordinates();
    assign_initial_coordinates();
    for (int i = 1; i <= 100; ++i) {
        double drag = 1.0 / i;
        for (int j = 1; j <= 100; ++j) {
            force_step(drag);
        }
    }
}

DataFrame Graph::get_node_positions() const
{
    CharacterVector label(nodes.size());
    NumericVector x(nodes.size()), y(nodes.size());
    for (int i = 0; i < nodes.size(); ++i) {
        const Node &n = nodes[i];
        label[i] = n.name;
        x[i] = n.get_x();
        y[i] = n.get_y();
    }
    DataFrame df =
        DataFrame::create(Named("label") = label,
                          Named("x") = x, Named("y") = y,
                          _["stringsAsFactors"] = false);
    df.attr("row.names") = label;
    return df;
}

DataFrame Graph::get_ggraph_nodes() const
{
    CharacterVector node_names(get_node_names());
    DataFrame df =
        DataFrame::create(Named("label") = node_names,
                          Named("is_leaf") = get_leaf_status(),
                          Named("stringsAsFactors") = false);
    df.attr("rown.names") = node_names;
    return df;
}

DataFrame Graph::get_ggraph_edges() const
{
    CharacterVector from(edges.size()), to(edges.size());
    CharacterVector edge_id(edges.size());
    LogicalVector admixture_edge(edges.size());
    for (int i = 0; i < edges.size(); ++i) {
        std::pair<unsigned int,unsigned int> edge = edges[i];
        const Node &parent = nodes[edge.first];
        const Node &child = nodes[edge.second];
        from[i] = parent.name;
        to[i] = child.name;
        edge_id[i] = parent.name + "_" + child.name;
        admixture_edge[i] = child.is_admixed();
    }
    DataFrame df =
        DataFrame::create(Named("from") = from,
                          Named("to") = to,
                          Named("edge.id") = edge_id,
                          Named("admixed") = admixture_edge,
                          _["stringsAsFactors"] = false);
    df.attr("row.names") = edge_id;
    return df;
}


RCPP_MODULE(Graph) {
    class_<Graph>("Graph")
        .constructor()

        .method("add_node", &Graph::add_node)
        .property("no_nodes", &Graph::get_no_nodes)
        .method("has_node", &Graph::has_node)
        .method("get_nodes", &Graph::get_node_names)
        .method("get_leaf_status", &Graph::get_leaf_status)

        .method("connect_nodes", &Graph::connect_nodes)

        .method("get_parents", &Graph::get_parents)
        .method("get_children", &Graph::get_children)

        .method("layout", &Graph::graph_layout)
        .method("get_node_positions", &Graph::get_node_positions)

        .method("get_ggraph_nodes", &Graph::get_ggraph_nodes)
        .method("get_ggraph_edges", &Graph::get_ggraph_edges)
    ;
}
