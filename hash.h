#ifndef HASH_H
#define HASH_H

#include <cstdint>
#include "types.h"

void hash_init();

u64 hash_piece  (int piece, int sq);
u64 hash_castle (int flags);
u64 hash_ep     (int sq);
u64 hash_side   (int side);

#endif
