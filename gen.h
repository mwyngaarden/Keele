#ifndef GEN_H
#define GEN_H

#include "types.h"
#include "pos.h"
#include "move.h"
#include "piece.h"

class Position;

constexpr int KnightIncrs[8] = { -33, -31, -18, -14,  14,  18,  31,  33 };
constexpr int BishopIncrs[4] = { -17, -15,  15,  17 };
constexpr int RookIncrs[4]   = { -16,  -1,   1,  16 };
constexpr int QueenIncrs[8]  = { -17, -16, -15,  -1,   1,  15,  16,  17 };

void gen_init();

std::size_t gen_pseudo_moves    (MoveList& moves, const Position& pos);
std::size_t gen_legal_moves     (MoveList& moves, const Position& pos);
std::size_t gen_evasion_moves   (MoveList& moves, const Position& pos);

int delta_incr      (int orig, int dest);

u8   pseudo_attack  (int incr);
bool pseudo_attack  (int orig, int dest, u8 piece);

int castle_flag(int sq);
    
#endif
