#include <array>
#include <chrono>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cstdint>
#include "gen.h"
#include "move.h"
#include "perft.h"
#include "pos.h"
using namespace std;

string move_to_string(const Gen::Move& move)
{
    ostringstream oss;

    oss << sq88_to_san(move.orig());
    oss << sq88_to_san(move.dest());

         if (move.promo_piece() == Piece::Knight)   oss << 'n';
    else if (move.promo_piece() == Piece::Bishop)   oss << 'b';
    else if (move.promo_piece() == Piece::Rook)     oss << 'r';
    else if (move.promo_piece() == Piece::Queen)    oss << 'q';

    return oss.str();
}

struct Perft {
    const char* fen;

    array<int64_t, 9> nodes;

    Perft(const char* f,
          int64_t n0 = 0,
          int64_t n1 = 0,
          int64_t n2 = 0,
          int64_t n3 = 0,
          int64_t n4 = 0,
          int64_t n5 = 0,
          int64_t n6 = 0,
          int64_t n7 = 0,
          int64_t n8 = 0)
        : fen(f)
    {
        nodes[0] = n0;
        nodes[1] = n1;
        nodes[2] = n2;
        nodes[3] = n3;
        nodes[4] = n4;
        nodes[5] = n5;
        nodes[6] = n6;
        nodes[7] = n7;
        nodes[8] = n8;
    }
};
            
int64_t perft(int depth, int pos, int64_t& illegal_moves, int64_t& ns);

std::vector<Perft> Fens {

    /*
    Perft("rnbq1bnr/pppp1ppp/3k4/3Pp3/8/1P6/P1P1PPPP/RNBQKBNR w KQ - 1 4", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("8/2p5/3p4/1P6/K3Pprk/1R6/6P1/8 b - e3 0 3", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("1R6/8/7k/4pPp1/8/4K3/5R2/8 w - g6 0 4", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("8/6p1/5k2/4pP2/8/8/1Q3Q2/2K5 w - e6 0 1", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("8/6p1/5k2/4pP2/8/8/1R3R2/2K5 w - e6 0 1", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("8/6p1/5k2/4pP2/8/8/1R3B2/2K5 w - e6 0 1", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("8/6p1/5k2/4pP2/8/8/1B3R2/2K5 w - e6 0 1", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Perft("rnbqkbnr/p3pppp/3p4/1Pp5/Q7/8/PP1PPPPP/RNB1KBNR w KQkq c6 0 4", 1, 1, 1, 1, 1, 1, 1, 1, 1),
    */

    Perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -", 20, 400, 8902, 197281, 4865609, 119060324, 3195901860, 123456789, 123456789),

    Perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", 48, 2039, 97862, 4085603, 193690690, 8031647685),

    Perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 14, 191, 2812, 43238, 674624, 11030083, 178633661, 3009794393),

    Perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 264, 9467, 422333, 15833292, 706045033),

    Perft("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1", 6, 264, 9467, 422333, 15833292, 706045033),

    Perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 44, 1486, 62379, 2103487, 89941194),

    Perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 46, 2079, 89890, 3894594, 164075551, 6923051137, 287188994746, 11923589843526, 490154852788714)
};

static int64_t perft(Position& pos,
                     int depth,
                     int64_t& illegal_moves,
                     int64_t& total_checks,
                     int64_t& dir_checks,
                     int64_t& rev_checks,
                     int64_t& dir_rev_checks,
                     int64_t& rev_rev_checks,
                     int64_t& mates
        );

int64_t perft(int depth, int pos, int64_t& illegal_moves, int64_t& ns)
{
    int64_t nodes = 0;
    int64_t total_checks = 0;
    int64_t dir_checks = 0;
    int64_t rev_checks = 0;
    int64_t dir_rev_checks = 0;
    int64_t rev_rev_checks = 0;
    int64_t mates = 0;
    int64_t nodes_diff = 0;

    for (int i = 0; i < int(Fens.size()); i++) {
        if (pos != -1 && i != pos)
            continue;

        auto pp = Fens[i];

        if (pp.nodes[depth - 1] == 0)
            continue;

        total_checks = 0;
        dir_checks = 0;
        rev_checks = 0;
        dir_rev_checks = 0;
        rev_rev_checks = 0;
        mates = 0;

        Position pos(pp.fen);

        cout << pos.dump() << endl;

        //cout << pp.fen << endl << endl;

        auto t0 = chrono::system_clock::now();

        int64_t have_nodes = perft(pos,
                                   depth,
                                   illegal_moves,
                                   total_checks,
                                   dir_checks,
                                   rev_checks,
                                   dir_rev_checks,
                                   rev_rev_checks,
                                   mates);
    
        auto t1 = chrono::system_clock::now();

        auto ns_local = chrono::duration_cast<chrono::microseconds>(t1 - t0);

        ns += ns_local.count();

        int64_t want_nodes = pp.nodes[depth - 1];

        nodes_diff += abs(want_nodes - have_nodes);

        nodes += have_nodes;

        int64_t cum_checks = dir_checks + rev_checks + dir_rev_checks + rev_rev_checks;
        int64_t double_checks = dir_rev_checks + rev_rev_checks;

        //cout << i << "," << want_nodes << "," << have_nodes << "," << nodes_diff << '\n';

        cout << "\twant nodes: " << want_nodes               << endl;
        cout << "\thave nodes: " << have_nodes               << endl;
        cout << "\tdiff nodes: " << have_nodes - want_nodes;

        if (want_nodes != have_nodes) cout << " !!!!!!!!!!!!!!!!";

        cout << endl;
        
        cout << "\tmates: "             << mates            << endl;
        cout << "\ttotal checks: "      << total_checks     << endl;
        cout << "\tdir checks: "        << dir_checks       << endl;
        cout << "\trev checks: "        << rev_checks       << endl;
        cout << "\tdir rev checks: "    << dir_rev_checks   << endl;
        cout << "\trev rev checks: "    << rev_rev_checks   << endl;
        cout << "\tdouble checks: "     << double_checks    << endl;
        cout << "\tcumulative: "        << cum_checks       << endl;
        cout << "\tdiff checks: "       << total_checks - cum_checks;

        if (total_checks != cum_checks) cout << " !!!!!!!!!!!!!!!!";

        cout << endl;
    }

    cout << endl;

    return nodes;
}

int64_t perft(Position& pos,
              int depth,
              int64_t& illegal_moves,
              int64_t& total_checks,
              int64_t& dir_checks,
              int64_t& rev_checks,
              int64_t& dir_rev_checks,
              int64_t& rev_rev_checks,
              int64_t& mates
              )
{
    if (depth == 0) {
        //bool sa_check = pos.side_attacks(pos.side() ^ 1, pos.king_sq());

        //total_checks        += sa_check;
        
        total_checks        += pos.last_move().is_check();
        dir_checks          += pos.last_move().is_dir_check();
        rev_checks          += pos.last_move().is_rev_check();
        dir_rev_checks      += pos.last_move().is_dir_rev_check();
        rev_rev_checks      += pos.last_move().is_rev_rev_check();

        return 1;
    }

    Gen::Move::List moves;

    int64_t legal_moves = 0;
    //int64_t total_moves = Gen::gen_pseudo_moves(moves, pos);
    
    Gen::gen_pseudo_moves(moves, pos);

    //if (depth == 1) return total_moves;
    
    //for (Gen::Move& m : moves) pos.note_move(m);

    for (const Gen::Move& m : moves) {
        Gen::Undo undo;

        pos.make_move(m, undo);
            
        int side = pos.side();

        if (!pos.side_attacks(side, pos.king_sq(side ^ 1))) {

            int64_t pmoves = perft(pos,
                                   depth - 1,
                                   illegal_moves,
                                   total_checks,
                                   dir_checks,
                                   rev_checks,
                                   dir_rev_checks,
                                   rev_rev_checks,
                                   mates);

            legal_moves += pmoves;
        }
        else
            ++illegal_moves;

        pos.unmake_move(m, undo);
    }

    mates += depth == 1 && !legal_moves;

    return legal_moves;
}

