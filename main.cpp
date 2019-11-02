#include <array>
#include <bitset>
#include <chrono>
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

    if (argc > 2 && string(argv[1]) == "-p") {
        int64_t illegal_moves = 0;

        auto t0 = chrono::system_clock::now();

        int64_t nodes = perft(stoi(argv[2]), false, illegal_moves);

        auto t1 = chrono::system_clock::now();

        auto ms = chrono::duration_cast<chrono::milliseconds>(t1 - t0);

        cout << "nodes: " << nodes << endl;
        cout << "milliseconds: " << ms.count() << endl;
        cout << "knps: " << double(nodes) / double(ms.count()) << endl;
        cout << "illegal moves: " << illegal_moves << " (" << 100.0 * illegal_moves / nodes << " %)" << endl;
    }

	return EXIT_SUCCESS;
}