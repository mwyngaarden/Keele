#ifndef HASH_H
#define HASH_H

#include <cstdint>

namespace Hash {

    void init();

    uint64_t key_piece      (int side, int piece, int sq);
    uint64_t key_castle     (int flags);
    uint64_t key_ep         (int sq);
    uint64_t key_side       (int side);

}

#endif
