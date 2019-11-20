#include <array>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cstdint>
#include "gen.h"
#include "move.h"
#include "perft.h"
#include "pos.h"
#include "string.h"
using namespace std;

struct FenInfo {
    string fen;

    array<int64_t, 9> nodes;

    FenInfo(const string& f,
            int64_t n0,
            int64_t n1,
            int64_t n2,
            int64_t n3,
            int64_t n4,
            int64_t n5,
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

static int64_t perft(Position& pos, int depth, int height, int64_t& illegal_moves, int64_t& mates);
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

            
int64_t perft(int depth, int64_t& illegal_moves, int64_t& total_microseconds, bool startpos)
{
    int64_t nodes = 0;

    vector<FenInfo> fen_info = read_epd("perft.epd");

    for (size_t i = 0; i < fen_info.size(); i++) {
        const FenInfo& fi = fen_info[i];

        Position pos(fi.fen);

        int64_t mates = 0;
        int64_t want_nodes = fi.nodes[depth - 1];

        auto t0 = chrono::high_resolution_clock::now();

        int64_t have_nodes = perft(pos, depth, 0, illegal_moves, mates);

        auto t1 = chrono::high_resolution_clock::now();

        auto microseconds = chrono::duration_cast<chrono::microseconds>(t1 - t0);

        total_microseconds += microseconds.count();
        
        int64_t diff_nodes = have_nodes - want_nodes;

        nodes += have_nodes;

        //cout << i << ',' << depth << ',' << want_nodes << ',' << have_nodes << ',' << diff_nodes << ',';

        //cout << (diff_nodes == 0 ? "PASS" : "FAIL") << '\n';

        /*
        cout << "\twant nodes: " << want_nodes               << endl;
        cout << "\thave nodes: " << have_nodes               << endl;
        cout << "\tdiff nodes: " << have_nodes - want_nodes;

        if (want_nodes != have_nodes) cout << " !!!!!!!!!!!!!!!!";

        cout << endl;
        
        cout << "\tmates: "             << mates            << endl;

        cout << endl;
        */

        if (startpos) break;
    }

    return nodes;
}

int64_t perft(Position& pos,
              int depth,
              int height,
              int64_t& illegal_moves,
              int64_t& mates)
{
    MoveList moves;

    int64_t legal_moves = 0;
    int64_t total_moves;

    if (pos.checkers() > 0)
        total_moves = gen_evasion_moves(moves, pos);
    else
        total_moves = gen_pseudo_moves(moves, pos);
    
    //if (depth == 1) return total_moves;
    
    for (const Move& m : moves) {
        bool move_is_legal = pos.move_is_legal(m);

        if (move_is_legal) {
            if (depth == 1)
                ++legal_moves;
            else {
                Undo undo;

                pos.make_move(m, undo);

                legal_moves += perft(pos, depth - 1, height + 1, illegal_moves, mates);

                pos.unmake_move(m, undo); 
            }
        }
        else
            ++illegal_moves;
    }

    if (depth == 1 && legal_moves == 0)
        mates++;

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

        int64_t n0 = stoll(list[1]);
        int64_t n1 = stoll(list[2]);
        int64_t n2 = stoll(list[3]);
        int64_t n3 = stoll(list[4]);
        int64_t n4 = stoll(list[5]);
        int64_t n5 = stoll(list[6]);
        int64_t n6 = list.size() >=  8 ? stoll(list[7]) : 0;
        int64_t n7 = list.size() >=  9 ? stoll(list[8]) : 0;
        int64_t n8 = list.size() >= 10 ? stoll(list[9]) : 0;

        fen_info.emplace_back(fen, n0, n1, n2, n3, n4, n5, n6, n7, n8);
    }

    return fen_info;
}

