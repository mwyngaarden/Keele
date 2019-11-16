#include <algorithm>
#include <array>
#include <bitset>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <utility>
#include <cassert>
#include <cstring>
#include "gen.h"
#include "hash.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
#include "string.h"
#include "types.h"
using namespace std;

void Position::init()
{
    fill(square_, square_ + 192, PieceInvalid256);

    for (int i = 0; i < 64; i++)
        square(to_sq88(i)) = PieceNone256;
}

Position::Position(const string& fen)
{
    init();

    Tokenizer fields(fen, ' ');

    assert(fields.size() >= 2);

    const string& pieces        = fields[0];
    const string& side          = fields[1];
    const string& flags         = fields.size() >= 3 ? fields[2] : "-";
    const string& ep_sq         = fields.size() >= 4 ? fields[3] : "-";
    const string& half_moves    = fields.size() >= 5 ? fields[4] : "0";
    const string& full_moves    = fields.size() >= 6 ? fields[5] : "1";

    // piece placement

    int rank = 7;
    int file = 0;

    for (char c : pieces) {
        switch (c) {
        case '/':
            rank--;
            file = 0;
            break;
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
            file += c - '0';
            break;
        default:
            add_piece(to_sq88(file, rank), char_to_piece256(c), true);
            file++;
            break;
        }
    }

    // active color

    if (side == "w")
        side_ = White;
    else if (side == "b")
        side_ = Black;

    // castling

    if (flags.find('K') != string::npos) flags_ |= WhiteCastleKFlag;
    if (flags.find('Q') != string::npos) flags_ |= WhiteCastleQFlag;
    if (flags.find('k') != string::npos) flags_ |= BlackCastleKFlag;
    if (flags.find('q') != string::npos) flags_ |= BlackCastleQFlag;

    // en passant
    
    if (ep_sq != "-") {
        ep_sq_ = san_to_sq88(ep_sq);

        assert(sq88_is_ok(ep_sq_));
    }

    // half moves counter

    if (half_moves != "-")
        half_moves_ = stoi(half_moves);

    // full moves counter
    
    if (full_moves != "-")
        full_moves_ = stoi(full_moves);

    // checkers

    set_checkers_slow();

    // key

    key_ ^= hash_castle(flags_);
    key_ ^= ep_is_valid() ? hash_ep(ep_sq_) : 0;
    key_ ^= hash_side(side_);

    assert(is_ok() == 0);
}

string Position::to_fen() const
{
    ostringstream oss;
    
    for (int rank = 7; rank >= 0; rank--) {
        int empty = 0;

        for (int file = 0; file < 8; file++) {
            u8 piece256 = square(to_sq88(file, rank));

            if (piece256 == PieceNone256)
                empty++;
            else {
                if (empty > 0) {
                    oss << to_string(empty);
                    empty = 0;
                }

                oss << piece256_to_char(piece256);
            }
        }

        if (empty > 0)
            oss << to_string(empty);

        if (rank > 0)
            oss << '/';
    }

    oss << ' ';

    oss << (side_ == White ? 'w' : 'b');

    oss << ' ';

    if ((flags_ & WhiteCastleKFlag) != 0) oss << 'K';
    if ((flags_ & WhiteCastleQFlag) != 0) oss << 'Q';
    if ((flags_ & BlackCastleKFlag) != 0) oss << 'k';
    if ((flags_ & BlackCastleQFlag) != 0) oss << 'q';
    if ((flags_ & CastleFlags) == 0) oss << '-';

    oss << ' ';

    oss << (ep_sq_ == SquareNone ? "-" : sq88_to_san(ep_sq_));

    oss << ' ';

    oss << half_moves_;

    oss << ' ';

    oss << full_moves_;

    return oss.str();
}

void Position::make_move(const Move& move, Undo& undo)
{
    undo.flags          = flags_;
    undo.ep_sq          = ep_sq_;
    undo.half_moves     = half_moves_;
    undo.full_moves     = full_moves_;
    undo.key            = key_;
    undo.checkers_sq[0] = checkers_sq_[0];
    undo.checkers_sq[1] = checkers_sq_[1];
    undo.checkers_count = checkers_count_;

    key_ ^= hash_castle(flags_);
    key_ ^= ep_is_valid() ? hash_ep(ep_sq_) : 0;
    key_ ^= hash_side(side_);

    int orig = move.orig();
    int dest = move.dest();

    int piece256 = square(orig);
        
    int inc = pawn_inc(side_);

    if (move.is_castle()) {
        int rook_orig = dest > orig ? orig + 3 : orig - 4;
        int rook_dest = dest > orig ? orig + 1 : orig - 1;

        mov_piece(orig, dest, true);
        mov_piece(rook_orig, rook_dest, true);
    }
    else if (move.is_ep()) {
        rem_piece(dest - inc, true);
        mov_piece(orig, dest, true);
    }
    else {
        if (move.is_capture()) {
            rem_piece(dest, true);

            flags_ &= castle_flag(dest);
        }
        
        if (move.is_promo()) {
            rem_piece(orig, true);
            add_piece(dest, to_piece256(side_, move.promo_piece()), true);
        }
        else
            mov_piece(orig, dest, true);
    }
        
    flags_ &= castle_flag(orig);
    ep_sq_ = move.is_double() ? dest - inc : SquareNone;
    half_moves_  = is_pawn(piece256) || move.is_capture() ? 0 : half_moves_ + 1;
    full_moves_ += side_;
    
    side_ = flip_side(side_);

    checkers_count_ = 0;

    set_checkers_fast(move);

    key_ ^= hash_castle(flags_);
    key_ ^= ep_is_valid() ? hash_ep(ep_sq_) : 0;
    key_ ^= hash_side(side_);
}

void Position::unmake_move(const Move& move, const Undo& undo)
{
    flags_          = undo.flags;
    ep_sq_          = undo.ep_sq;
    half_moves_     = undo.half_moves;
    full_moves_     = undo.full_moves;
    key_            = undo.key;
    checkers_sq_[0] = undo.checkers_sq[0];
    checkers_sq_[1] = undo.checkers_sq[1];
    checkers_count_ = undo.checkers_count;

    const int orig = move.orig();
    const int dest = move.dest();

    if (move.is_castle()) {
        int rook_orig = dest > orig ? orig + 3 : orig - 4;
        int rook_dest = dest > orig ? orig + 1 : orig - 1;

        mov_piece(dest, orig, false);
        mov_piece(rook_dest, rook_orig, false);
    }
    else if (move.is_ep()) {
        const int inc = pawn_inc(side_);

        mov_piece(dest, orig, false);
        add_piece(dest + inc, make_pawn(side_), false);
    }
    else {
        if (move.is_promo()) {
            rem_piece(dest, false);
            add_piece(orig, make_pawn(flip_side(side_)), false);
        }
        else
            mov_piece(dest, orig, false);

        if (move.is_capture())
            add_piece(dest, move.capture_piece(), false);
    }

    side_ = flip_side(side_);
}

int Position::is_ok(bool incheck) const
{
    if (!side_is_ok(side_))
        return __LINE__;
        
    if (piece_list_[WhiteKing12].size() != 1) return __LINE__;
    if (piece_list_[BlackKing12].size() != 1) return __LINE__;
    
    size_t type_min[6] = { 0,  0,  0,  0, 0, 1 };
    size_t type_max[6] = { 8, 10, 10, 10, 9, 1 };

    size_t sq_count[128] = { };

    for (int side : { White, Black }) {
        size_t color_count = 0;

        for (int piece = Pawn; piece <= King; piece++) {
            size_t type_count = 0;

            const int p12 = to_piece12(side, piece);

            for (const int sq : piece_list(p12)) {
                if (!sq88_is_ok(sq))
                    return __LINE__;

                u8 piece256 = to_piece256(side, piece);

                if (!piece256_is_ok(piece256))
                    return __LINE__;
                
                if (square(sq) != piece256)
                    return __LINE__;

                sq_count[sq]++;

                color_count++;
                type_count++;
            }

            if (type_count != piece_list_[p12].size())
                return __LINE__;

            if (type_count < type_min[piece]) return __LINE__;
            if (type_count > type_max[piece]) return __LINE__;
        }

        if (color_count > 16)
            return __LINE__;
    }

    for (int i = 0; i < 64; i++) {
        int sq = to_sq88(i);

        int plist_count = sq_count[sq];
        int board_count = square(sq) != PieceNone256;

        if (plist_count != board_count)
            return __LINE__;
    }

    if (incheck && side_attacks(side_, king_sq(flip_side(side_))))
        return __LINE__;

    if (flags_ < 0 && flags_ >= 16)
        return __LINE__;

    if (ep_sq_ != SquareNone) {
        int rank = sq88_rank(ep_sq_, flip_side(side_));

        if (rank != Rank3)
            return __LINE__;

        u8 opawn = make_pawn(flip_side(side_));

        int inc = pawn_inc(flip_side(side_));

        if (square(ep_sq_ + inc) != opawn)
            return __LINE__;
    }

    return 0;
}

void Position::add_piece(int sq, u8 piece256, bool update_key)
{
    assert(sq88_is_ok(sq));
    assert(is_empty(sq));
    assert(piece256_is_ok(piece256));

    const int p12 = to_piece12(piece256);

    assert(piece12_is_ok(p12));

    piece_list_[p12].add(sq);

    if (update_key)
        key_ ^= hash_piece(p12, sq);

    square(sq) = piece256;
}

void Position::rem_piece(int sq, bool update_key)
{
    assert(sq88_is_ok(sq));
    assert(!is_empty(sq));

    u8 piece256 = square(sq);
    
    assert(piece256_is_ok(piece256));

    const int p12 = to_piece12(piece256);
    
    assert(piece12_is_ok(p12));
    
    piece_list_[p12].remove(sq);
   
    if (update_key)
        key_ ^= hash_piece(p12, sq);

    square(sq) = PieceNone256;
}

void Position::mov_piece(int orig, int dest, bool update_key)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));
    assert(!is_empty(orig));
    assert(is_empty(dest));

    u8 piece256 = square(orig);

    assert(piece256_is_ok(piece256));

    const int p12 = to_piece12(piece256);

    assert(piece12_is_ok(p12));
    
    piece_list_[p12].replace(orig, dest);

    if (update_key) {
        key_ ^= hash_piece(p12, orig);
        key_ ^= hash_piece(p12, dest);
    }

    swap(square(orig), square(dest));
}

bool Position::side_attacks(int side, int dest) const
{
    assert(side_is_ok(side));
    assert(sq88_is_ok(dest));

    int king = king_sq(side);

    if (pseudo_attack(king, dest, KingFlag256))
        return true;

    u8 pawn256 = make_pawn(side);

    {
        int inc = pawn_inc(side);

        if (int orig = dest - inc - 1; square(orig) == pawn256) return true;
        if (int orig = dest - inc + 1; square(orig) == pawn256) return true;
    }

    for (const int orig : piece_list(WhiteKnight12 + side)) {
        if (delta_type(orig, dest) & KnightFlag256)
            return true;
    }

    for (const int orig : piece_list(WhiteBishop12 + side)) {
        if (!(delta_type(orig, dest) & BishopFlag256))
            continue;

        int inc = delta_inc(dest, orig);
        int sq = dest;

        do { sq += inc; } while (square(sq) == PieceNone256);

        if (sq == orig)
            return true;
    }

    for (const int orig : piece_list(WhiteRook12 + side)) {
        if (!(delta_type(orig, dest) & RookFlag256))
            continue;

        int inc = delta_inc(dest, orig);
        int sq = dest;

        do { sq += inc; } while (square(sq) == PieceNone256);

        if (sq == orig)
            return true;
    }
    
    for (const int orig : piece_list(WhiteQueen12 + side)) {
        if (!(delta_type(orig, dest) & QueenFlags256))
            continue;

        int inc = delta_inc(dest, orig);
        int sq = dest;

        do { sq += inc; } while (square(sq) == PieceNone256);

        if (sq == orig)
            return true;
    }

    return false;
}

bool Position::piece_attacks(int orig, int dest) const
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));
    assert(!is_empty(orig));

    const u8 piece256 = square(orig);

    assert(piece256_is_ok(piece256));

    if (!pseudo_attack(orig, dest, piece256))
        return false;

    return !is_slider(piece256) || is_empty(orig, dest);
}

string Position::dump() const
{
    ostringstream oss;

    oss << "FEN: " << to_fen()
        << endl
        << "Key: 0x" << setfill('0') << setw(16) << hex << key_
        << endl
        << endl
        << "    +---+---+---+---+---+---+---+---+"
        << endl;
   
    for (int rank = 7; rank >= 0; rank--) {
        oss << ' ' << (rank + 1) << "  |";

        for (int file = 0; file <= 7; file++) { 
            u8 piece256 = square(to_sq88(file, rank));

            if (is_white(piece256))
                oss << '-' << piece256_to_char(piece256) << '-';
            else if (is_black(piece256))
                oss << '<' << piece256_to_char(piece256) << '>';
            else  {
                int clr = ~(file ^ rank) & 1;

                oss << ' ' << (clr ? '.' : ' ') << ' ';
            }

            oss << '|';
        }

        oss << endl
            << "    +---+---+---+---+---+---+---+---+"
            << endl;
    }

    oss << "      a   b   c   d   e   f   g   h  "
        << endl;

    return oss.str();
}

uint64_t Position::calc_key() const
{
    uint64_t key = 0;

    for (int i = 0; i < 64; i++) {
        int sq = to_sq88(i);

        u8 piece256 = square(sq);

        if (piece256 == PieceNone256)
            continue;

        assert(piece256_is_ok(piece256));

        int p12 = to_piece12(piece256);

        key ^= hash_piece(p12, sq);
    }

    key ^= hash_castle(flags_);
    key ^= ep_is_valid() ? hash_ep(ep_sq_) : 0;
    key ^= hash_side(side_);

    return key;
}

bool Position::ep_is_valid() const
{
    if (ep_sq_ == SquareNone)
        return false;

    int sq = ep_sq_ - pawn_inc(side_);

    u8 mpawn = make_pawn(side_);

    return square(sq - 1) == mpawn || square(sq + 1) == mpawn;
}

void Position::mark_pins(bitset<128>& pins) const
{
    assert(pins.none());

    const int king = king_sq();

    const int mside = side_;
    const int oside = flip_side(side_);

    const u8 mflag = make_flag(mside);

    for (int p12 = WhiteBishop12 + oside; p12 <= BlackQueen12; p12 += 2) {
        for (const int orig : piece_list_[p12]) {
            assert(sq88_is_ok(orig));

            if (!(square(orig) & delta_type(king, orig)))
                continue;

            const int inc = delta_inc(king, orig);
            int sq1 = king;
            int sq2 = orig;

            do { sq1 += inc; } while (square(sq1) == PieceNone256);
            
            assert(sq88_is_ok(sq1));

            if (!(square(sq1) & mflag))
                continue;

            do { sq2 -= inc; } while (square(sq2) == PieceNone256);

            assert(sq88_is_ok(sq2));

            if (sq1 != sq2)
                continue;

            assert(!pins.test(sq1));

            pins.set(sq1);
        }
    }
}

void Position::set_checkers_slow()
{
    assert(checkers_count_ == 0);

    const int king = king_sq();

    const int oside = flip_side(side_);

    {
        const u8 opawn = make_pawn(oside);

        const int inc = pawn_inc(oside);

        if (int orig = king - inc - 1; square(orig) == opawn) checkers_sq_[checkers_count_++] = orig;
        if (int orig = king - inc + 1; square(orig) == opawn) checkers_sq_[checkers_count_++] = orig;
    }

    for (int p12 = WhiteKnight12 + oside; p12 <= BlackQueen12; p12 += 2)
        for (const int orig : piece_list_[p12])
            if (piece_attacks(orig, king))
                checkers_sq_[checkers_count_++] = orig;
}

void Position::set_checkers_fast(const Move& move)
{
    assert(checkers_count_ == 0);

    const int king = king_sq();

    const int orig = move.orig();
    const int dest = move.dest();
    
    if (move.is_castle()) {
        int rook = dest > orig ? orig + 1 : orig - 1;

        if (piece_attacks(rook, king))
            checkers_sq_[checkers_count_++] = rook;

        return;
    }
    
    if (move.is_ep()) {
        set_checkers_slow();

        return;
    }
    
    const int inc = delta_inc(king, orig);

    // revealed check?

    if (inc != delta_inc(king, dest)) {
        if (delta_type(inc) & QueenFlags256) {
            const u8 oflag = make_flag(flip_side(side_));

            int sq = king + inc;
            
            u8 piece256;

            while ((piece256 = square(sq)) == PieceNone256) sq += inc;

            if ((piece256 & oflag) && pseudo_attack(sq, king, piece256))
                checkers_sq_[checkers_count_++] = sq;
        }
    }

    // direct check?

    if (piece_attacks(dest, king))
        checkers_sq_[checkers_count_++] = dest;
}

bool Position::is_empty(int orig, int dest) const
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    assert(pseudo_attack(orig, dest, QueenFlags256));

    const int inc = delta_inc(orig, dest);

    int sq = orig;

    do {
        sq += inc;

        if (sq == dest) return true;
    } while (square(sq) == PieceNone256);

    return false;
}
