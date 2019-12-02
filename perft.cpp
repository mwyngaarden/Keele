#include <array>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <cstdint>
#include <x86intrin.h>
#include "gen.h"
#include "move.h"
#include "perft.h"
#include "pos.h"
#include "string.h"
#include "types.h"
using namespace std;

struct FenInfo {
    string fen;

    array<i64, 9> nodes;

    FenInfo(const string& f,
            i64 n0,
            i64 n1,
            i64 n2,
            i64 n3,
            i64 n4,
            i64 n5,
            i64 n6 = 0,
            i64 n7 = 0,
            i64 n8 = 0)
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

static i64 perft(Position& pos, int depth, int height, i64& illegal_moves);

static vector<FenInfo> read_epd(const string& filename);

string move_to_string(const Move& move)
{
    ostringstream oss;

    oss << sq88_to_san(move.orig());
    oss << sq88_to_san(move.dest());

         if (move.promo_piece() == Knight)   oss << 'n';
    else if (move.promo_piece() == Bishop)   oss << 'b';
    else if (move.promo_piece() == Rook)     oss << 'r';
    else if (move.promo_piece() == Queen)    oss << 'q';

    return oss.str();
}

            
i64 perft(int depth, i64& illegal_moves, i64& total_microseconds, i64& total_cycles, bool startpos)
{
    i64 nodes = 0;

    vector<FenInfo> fen_info = read_epd("perft.epd");

    i64 invalid = 0;

    for (size_t i = 0; i < fen_info.size(); i++) {
        const FenInfo& fi = fen_info[i];

        Position pos(fi.fen);

        i64 want_nodes = fi.nodes[depth - 1];

        auto t0 = chrono::high_resolution_clock::now();
        
        i64 rdtsc0 = __rdtsc();

        i64 have_nodes = perft(pos, depth, 0, illegal_moves);

        i64 rdtsc1 = __rdtsc();

        auto t1 = chrono::high_resolution_clock::now();

        auto microseconds = chrono::duration_cast<chrono::microseconds>(t1 - t0);

        i64 cycles = rdtsc1 - rdtsc0;

        double cpn = double(cycles) / double(have_nodes);

        total_microseconds += microseconds.count();
        total_cycles += cycles;
        
        i64 diff_nodes = have_nodes - want_nodes;

        invalid += diff_nodes != 0;

        nodes += have_nodes;
        
        double cum_mnps = double(nodes) / (double(total_microseconds) / 1e+6) / 1e+6;

        if ((i + 1) % 100 == 0) {
            stringstream ss;

            ss  << setprecision(1) << fixed 
                << "n = "      << setw(4) << (i + 1)            << ' '
                << "d = "      << setw(1) << depth              << ' '
                //<< "wn = "     << want_nodes       << ' '
                //<< "hn = "     << have_nodes       << ' '
                << "dn = "     << setw(1) << diff_nodes         << ' '
                << "inv = "    << setw(1) << invalid            << ' '
                << "cpn = "    << setw(5) << cpn                << ' '
                << "cmnps = " << setw(5) << cum_mnps           << ' '
                << (diff_nodes == 0 ? "PASS" : "FAIL");

            cout << ss.str() << endl;
        }

        if (startpos) break;
    }

    return nodes;
}

i64 perft(Position& pos, int depth, int height, i64& illegal_moves)
{
    if (depth == 0) return 1;

    MoveList moves;

    i64 legal_moves = 0;
    i64 total_moves = gen_legal_moves(moves, pos);

    if (GenerateLegal && !Debug && depth == 1) return total_moves;

    for (const auto& m : moves) {
        if (!GenerateLegal && !pos.move_is_legal(m)) {
            illegal_moves++;
            continue;
        }

        Undo undo;

        pos.make_move(m, undo);

        assert(!pos.side_attacks(pos.side(), pos.king_sq(flip_side(pos.side()))));

        legal_moves += perft(pos, depth - 1, height + 1, illegal_moves);

        pos.unmake_move(m, undo); 
    }

    return legal_moves;
}

vector<FenInfo> read_epd(const string& filename)
{
    vector<FenInfo> fen_info;

    ifstream ifs(filename);

    string line;

    while (getline(ifs, line)) {
        assert(!line.empty());

        Tokenizer list(line, ',');

        assert(list.size() >= 7);

        const string& fen = list[0];

        i64 n0 = stoll(list[1]);
        i64 n1 = stoll(list[2]);
        i64 n2 = stoll(list[3]);
        i64 n3 = stoll(list[4]);
        i64 n4 = stoll(list[5]);
        i64 n5 = stoll(list[6]);
        i64 n6 = list.size() >=  8 ? stoll(list[7]) : 0;
        i64 n7 = list.size() >=  9 ? stoll(list[8]) : 0;
        i64 n8 = list.size() >= 10 ? stoll(list[9]) : 0;

        fen_info.emplace_back(fen, n0, n1, n2, n3, n4, n5, n6, n7, n8);
    }

    return fen_info;
}

