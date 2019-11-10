#include <array>
#include <bitset>
#include <iostream>
#include <vector>
#include <cstdlib>
#include "gen.h"
#include "perft.h"
#include "piece.h"
#include "pos.h"
#include "square.h"
using namespace std;

int main(int argc, char *argv[])
{
    Piece::init();
    Gen::init();

    assert(argc == 3);

    int depth = stoi(argv[1]); 
    int pos = stoi(argv[2]); 
    
    int64_t illegal_moves = 0;
    int64_t ns = 0;

    int64_t nodes = perft(depth, pos, illegal_moves, ns);

    cout << "nodes: " << nodes << endl;
    cout << "milliseconds: " << double(ns) / 1000.0 << endl;
    cout << "knps: " << 1000.0 * double(nodes) / double(ns) << endl;
    cout << "illegal moves: " << illegal_moves << " (" << 100.0 * illegal_moves / nodes << " %)" << endl;

	return EXIT_SUCCESS;
}
