#include <cassert>
#include "eval.h"
using namespace std;

int value_to_tt(int value, int ply)
{
    assert(value != ValueNone);

    if (value >= ValueMateInPliesMax)
        return value + ply;
    else if (value <= ValueMatedInPliesMax)
        return value - ply;
    else
        return value;
}

int value_from_tt(int value, int ply, int half_moves)
{
    if (value == ValueNone)
        return ValueNone;
    else if (value >= ValueMateInPliesMax)
        return ValueMate - value > 99 - half_moves ? ValueMateInPliesMax : value - ply;
    else if (value <= ValueMatedInPliesMax)
        return ValueMate + value > 99 - half_moves ? ValueMatedInPliesMax : value + ply;
    else
        return value;
}

