#ifndef GEN_H
#define GEN_H

#include <array>
#include "move.h"
#include "piece.h"
#include "pos.h"

namespace Gen {

    void init();

    constexpr int KnightIncs[8] = { -33, -31, -18, -14,  14,  18,  31,  33 };
    constexpr int BishopIncs[4] = { -17, -15,  15,  17 };
    constexpr int RookIncs[4]   = { -16,  -1,   1,  16 };
    constexpr int QueenIncs[8]  = { -17, -16, -15,  -1,   1,  15,  16,  17 };

    std::size_t gen_pseudo_moves    (Move::List& moves, const Position& pos);
    std::size_t gen_legal_moves     (Move::List& moves, const Position& pos);
    std::size_t gen_king_evasions   (Move::List& moves, const Position& pos);

    int             delta_inc   (int orig, int dest);
    Piece::Piece256 delta_type  (int orig, int dest);

    int castle_flag(int sq);
    
}

#endif
