#ifndef HASH_H
#define HASH_H

#include <cstdint>

namespace Hash {

    void init();

    uint64_t hash_piece     (int piece, int sq);
    uint64_t hash_castle    (int flags);
    uint64_t hash_ep        (int sq);
    uint64_t hash_side      (int side);

}

#endif
