#include <iostream>
#include <string>
#include <vector>
#include <cstdint>
#include "gen.h"
#include "move.h"
#include "pos.h"
using namespace std;

struct Perft {
    string fen;
    vector<int64_t> nodes;

    Perft(string f, vector<int64_t> n) : fen(f), nodes(n) { }
};

static int64_t perft(Position& pos, int depth, bool divide, int64_t& illegal_moves);

int64_t perft(int depth, bool divide, int64_t& illegal_moves)
{
    vector<Perft> perft_pos {

        Perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
              { 20, 400, 8902, 197281, 4865609, 119060324, 3195901860 }),

        Perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -",
              { 48, 2039, 97862, 4085603, 193690690, 8031647685 }),

        Perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -",
              { 14, 191, 2812, 43238, 674624, 11030083, 178633661, 3009794393 }),

        Perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
              { 6, 264, 9467, 422333, 15833292, 706045033 }),

        Perft("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
              { 6, 264, 9467, 422333, 15833292, 706045033 }),

        Perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
              { 44, 1486, 62379, 2103487, 89941194 }),

        Perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
              { 46, 2079, 89890, 3894594, 164075551, 6923051137, 287188994746, 11923589843526, 490154852788714 })

    };
    
    int64_t nodes = 0;

#if 1

    for (auto& pp : perft_pos) {
        if (depth > pp.nodes.size())
            continue;

        Position pos(pp.fen);

        cout << pp.fen << endl;
        cout << pos.get_fen() << endl;

        int64_t have_nodes = perft(pos, depth, divide, illegal_moves);
        int64_t want_nodes = pp.nodes[depth - 1];

        nodes += have_nodes;

        cout << " want nodes: " << want_nodes
             << " have nodes: " << have_nodes
             << " diff nodes: " << have_nodes - want_nodes
             << endl << endl;
    }

#else

    auto pp = perft_pos[5];

    Position pos(pp.fen);

    cout << pp.fen << endl;
    cout << pos.get_fen() << endl;

    int64_t have_nodes = perft(pos, depth, divide);
    int64_t want_nodes = pp.nodes[depth - 1];

    cout << " want nodes: " << want_nodes
         << " have nodes: " << have_nodes
         << " diff nodes: " << have_nodes - want_nodes
         << endl;

#endif

    return nodes;
}

int64_t perft(Position& pos, int depth, bool divide, int64_t& illegal_moves)
{
    if (depth == 0)
        return 1;

    int64_t legal_moves = 0;
    int total_moves = 0;
    char pp[6] = { '!', 'n', 'b', 'r', 'q', '!' };

    Gen::Move moves[256];
    Gen::Move* m = Gen::add_pseudo_moves(moves, pos);
    total_moves = m - moves;

    for (int i = 0; i < total_moves; i++) {
        Gen::Undo undo;

        pos.make_move(moves[i], undo);

        if (divide) {
            cout << sq88_to_san(moves[i].orig());
            cout << sq88_to_san(moves[i].dest());
            if (moves[i].is_promo()) cout << pp[moves[i].promo_piece()];
            cout << ' ';
        }

        int side = pos.side();

        if (!pos.side_attacks(side, pos.king_sq(side ^ 1))) {
            int64_t pmoves = perft(pos, depth - 1, false, illegal_moves);

            if (divide)
                cout << pmoves << endl;

            legal_moves += pmoves;
        }
        else
            ++illegal_moves;

        pos.unmake_move(moves[i], undo);
    }

    return legal_moves;
}

