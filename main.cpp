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

struct Pos {
    const char* fen;
    uint64_t key;

    Pos(const char* f, uint64_t k) : fen(f), key(k) { }
};

void validate_hash()
{
    vector<Pos> fens {
        Pos("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 0x463b96181691fc9cull),
        Pos("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", 0x823c9b50fd114196ull),
        Pos("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", 0x0756b94461c50fb0ull),
        Pos("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2", 0x662fafb965db29d4ull),
        Pos("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3", 0x22a48b5a8e47ff78ull),
        Pos("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3", 0x652a607ca3f242c1ull),
        Pos("rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 0 4", 0x00fdd303c946bdd9ull),
        Pos("rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3", 0x3c8123ea7b067637ull),
        Pos("rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 0 4", 0x5c3f9b829b279560ull)
    };

    for (size_t i = 0; i < fens.size(); i++) {
        Position pos(fens[i].fen);
        
        cout << hex;
        cout << "  have key: " << pos.key() << endl;
        cout << "  want key: " << fens[i].key << endl << endl;
    }
}

int main(int argc, char *argv[])
{
    Piece::init();
    Gen::init();
    Hash::init();

    if (argc != 3) {
        validate_hash();

        return EXIT_SUCCESS;
    }

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


