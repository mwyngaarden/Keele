#ifndef SEARCH_H
#define SEARCH_H

#include <vector>
#include "types.h"
#include "pos.h"
#include "move.h"

class SearchContext {
public:
    enum Limit : int { Infinite, Nodes, Time, Depth };

    static constexpr i64 NodesCountdownCycle = 1024;

    bool complete() const;
    void countdown() { nodes_countdown_--; }
    bool checkup();

private:
    Limit limit_;

    int depth_max_ = 0;
    i64 nodes_max_ = 0;
    i64 time_max_ = 0;

    i64 begin_time_ = 0;
    i64 end_time_ = 0;

    i64 nodes_countdown_ = NodesCountdownCycle;
};

struct SearchStack {
    Move* pv;

    int ply;

    Move curr_move;
    Move excl_move;

    Move killers[2];

    int static_eval;

    int move_count;
};

struct RootMove {
    int curr_score = -ValueInf;
    int prev_score = -ValueInf;

    int sel_depth = 0;

    int best_move_count = 0;

    std::vector<Move> pv;
};

void search_init();

//void search_root(SearchContext &ctx, Position& pos);

//int search(bool pv_node, Position& pos, SearchStack* ss, int alpha, int beta, int depth, bool cut_node);

#endif
