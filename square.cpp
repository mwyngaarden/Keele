#include <sstream>
#include <string>
#include <cassert>
#include <cstdint>
#include "square.h"
using namespace std;

int san_to_sq88(string s)
{
    assert(s.size() == 2);

    int file = s[0] - 'a';
    int rank = s[1] - '1';

    assert(file_is_ok(file));
    assert(rank_is_ok(rank));

    int sq = to_sq88(file, rank);

    assert(is_sq88(sq));

    return sq;
}

string sq88_to_san(int sq)
{
    assert(is_sq88(sq));

    int file = sq88_file(sq);
    int rank = sq88_rank(sq);
    
    assert(file_is_ok(file));
    assert(rank_is_ok(rank));

    ostringstream oss;

    oss << static_cast<char>('a' + file);
    oss << static_cast<char>('1' + rank);

    return oss.str();
}
