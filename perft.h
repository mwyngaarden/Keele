#ifndef PERFT_H
#define PERFT_H

#include <array>
#include <vector>
#include <cstdint>

int64_t perft(int depth, int64_t& illegal_moves, int64_t& total_microseconds, int64_t& total_cycles, bool startpos);

#endif
