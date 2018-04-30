#ifndef NODES_HPP
#define NODES_HPP

#include <string>

struct Node {
    std::string name;
    Node(std::string &name) : name(name) {}
};

#endif
