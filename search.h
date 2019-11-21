#ifndef SEARCH_H
#define SEARCH_H

#include "pos.h"

class SearchContext {
public:
    enum Limit : int { Infinite, Nodes, Time, Depth };

    static constexpr int64_t NodesCountdownCycle = 1024;

    bool complete() const;
    void countdown() { nodes_countdown_--; }
    bool checkup();

private:
    Limit limit_;

    int depth_max_ = 0;
    int64_t nodes_max_ = 0;
    int64_t time_max_ = 0;

    int64_t begin_time_ = 0;
    int64_t end_time_ = 0;

    int64_t nodes_countdown_ = NodesCountdownCycle;
};

void search_init();

void search_root(SearchContext &ctx, Position& pos);

#endif
