#ifndef GEN_H
#define GEN_H

#include <array>
#include "move.h"
#include "piece.h"
#include "pos.h"

namespace Gen {

    void init();

    constexpr int piece_dirs[6][8] = {
        { },
        { -33, -31, -18, -14,  14,  18,  31,  33 },
        { -17, -15,  15,  17 },
        { -16,  -1,   1,  16 },
        { -17, -16, -15,  -1,   1,  15,  16,  17 },
        { -17, -16, -15,  -1,   1,  15,  16,  17 }
    };

    Move* add_pseudo_moves  (Move* moves, const Position& pos);
    Move* add_legal_moves   (Move* moves, const Position& pos);
    Move* add_evasion_moves (Move* moves, const Position& pos);

    int move_dir(int orig, int dest);
    Piece::Piece256 move_flag(int orig, int dest);

    int castle_flag(int orig);
    
}

#endif
