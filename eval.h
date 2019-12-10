#ifndef EVAL_H
#define EVAL_H

#include "types.h"

int value_to_tt(int value, int ply);
int value_from_tt(int value, int ply, int half_moves);

#endif
