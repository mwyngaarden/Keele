#ifndef PERFT_H
#define PERFT_H

#include <array>
#include <vector>
#include <cstdint>

int64_t perft(int depth, int pos, int64_t& illegal_moves, int64_t& ns);

#endif
