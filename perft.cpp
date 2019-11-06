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

static int64_t perft(Position& pos,
                     int depth,
                     int64_t& illegal_moves,
                     int64_t& checks,
                     int64_t& discovered_checks,
                     int64_t& double_checks,
                     int64_t& ep,
                     int64_t& mates,
                     int64_t& castles,
                     int64_t& captures,
                     int64_t& promos
        );

int64_t perft(int depth, int pos, int64_t& illegal_moves)
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
    int64_t checks = 0;
    int64_t discovered_checks = 0;
    int64_t double_checks = 0;
    int64_t ep = 0;
    int64_t mates = 0;
    int64_t castles = 0;
    int64_t captures = 0;
    int64_t promos = 0;

    for (int i = 0; i < perft_pos.size(); i++) {
        if (pos != -1 && i != pos)
            continue;

        auto pp = perft_pos[i];

        if (depth > pp.nodes.size())
            continue;

        checks = 0;
        discovered_checks = 0;
        double_checks = 0;
        ep = 0;
        mates = 0;
        castles = 0;
        captures = 0;
        promos = 0;

        Position pos(pp.fen);

        cout << pos.dump() << endl << endl;

        cout << pp.fen << endl;

        int64_t have_nodes = perft(pos,
                                   depth,
                                   illegal_moves,
                                   checks,
                                   discovered_checks,
                                   double_checks,
                                   ep,
                                   mates,
                                   castles,
                                   captures,
                                   promos
                );

        int64_t want_nodes = pp.nodes[depth - 1];

        nodes += have_nodes;

        cout << "\twant nodes: " << want_nodes               << endl;
        cout << "\thave nodes: " << have_nodes               << endl;
        cout << "\tdiff nodes: " << have_nodes - want_nodes  << endl;
        cout << "\tchecks: "     << checks                   << endl;
        cout << "\tdsc checks: " << discovered_checks        << endl;
        cout << "\tdbl checks: " << double_checks            << endl;
        cout << "\tep: "         << ep                       << endl;
        cout << "\tmates: "      << mates                    << endl;
        cout << "\tcastles: "    << castles                  << endl;
        cout << "\tcaptures: "   << captures                 << endl;
        cout << "\tpromos: "     << promos                   << endl;

        cout << endl;
    }

    cout << endl;

    return nodes;
}

int64_t perft(Position& pos,
              int depth,
              int64_t& illegal_moves,
              int64_t& checks,
              int64_t& discovered_checks,
              int64_t& double_checks,
              int64_t& ep,
              int64_t& mates,
              int64_t& castles,
              int64_t& captures,
              int64_t& promos
        )
{
    if (depth == 0) {
        checks              += pos.last_move().is_check();

        if (   pos.last_move().is_double_check()
            || pos.last_move().is_ep_double_special())
            double_checks++;
        
        if (    pos.last_move().is_rev_check()
            && !pos.last_move().is_double_check()
            && !pos.last_move().is_ep_double_special())
            discovered_checks++;

        ep                  += pos.last_move().is_ep();
        castles             += pos.last_move().is_castle();
        captures            += pos.last_move().is_capture();
        promos              += pos.last_move().is_promo();

        return 1;
    }

    Gen::Move::List moves;

    int64_t legal_moves = 0;
    int64_t total_moves = Gen::gen_pseudo_moves(moves, pos);

    for (Gen::Move& m : moves)
        pos.note_move(m);

    if (depth == 1) return total_moves;

    for (const Gen::Move& m : moves) {
        Gen::Undo undo;

        pos.make_move(m, undo);

        int side = pos.side();

        if (!pos.side_attacks(side, pos.king_sq(side ^ 1))) {
        //if (pos.move_was_legal()) {

            int64_t pmoves = perft(pos, depth - 1, illegal_moves, checks, discovered_checks, double_checks, ep, mates, castles, captures, promos);

            legal_moves += pmoves;
        }
        else
            ++illegal_moves;

        pos.unmake_move(m, undo);
    }

    mates += depth == 1 && !legal_moves;

    return legal_moves;
}

