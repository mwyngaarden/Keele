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
using namespace std;

void Position::init()
{
    fill(square_, square_ + 192, Piece::PieceInvalid256);

    for (int i = 0; i < 64; i++)
        square(to_sq88(i)) = Piece::PieceNone256;
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
            add_piece(to_sq88(file, rank), Piece::to_piece256(c), true);
            file++;
            break;
        }
    }

    // active color

    if (side == "w")
        side_ = Piece::White;
    else if (side == "b")
        side_ = Piece::Black;

    // castling

    if (flags.find('K') != string::npos) flags_ |= WhiteCastleKFlag;
    if (flags.find('Q') != string::npos) flags_ |= WhiteCastleQFlag;
    if (flags.find('k') != string::npos) flags_ |= BlackCastleKFlag;
    if (flags.find('q') != string::npos) flags_ |= BlackCastleQFlag;

    // en passant
    
    if (ep_sq != "-") {
        ep_sq_ = san_to_sq88(ep_sq);

        assert(is_sq88(ep_sq_));
    }

    // half moves counter

    if (half_moves != "-")
        half_moves_ = stoi(half_moves);

    // full moves counter
    
    if (full_moves != "-")
        full_moves_ = stoi(full_moves);

    // set last move

    if (ep_sq_ != SquareNone) {
        int inc = pawn_inc(Piece::flip_side(side_));

        [[maybe_unused]] Piece::Piece256 opawn = Piece::make_pawn(Piece::flip_side(side_));
        
        assert(square(ep_sq_ + inc) == opawn);

        last_move_ = Gen::Move(ep_sq_ - inc, ep_sq_ + inc) | Gen::Move::DoubleFlag;
    }

    int king = king_sq();

    int oside = Piece::flip_side(side_);

    int checker1 = SquareNone;
    int checker2 = SquareNone;

    for (int p12 = Piece::WhitePawn12 + oside; p12 <= Piece::BlackQueen12; p12 += 2) {
        for (const int orig : piece_list(p12)) {
            if (piece_attacks(orig, king)) {
                assert(checker2 == SquareNone);
                
                checker2 = checker1;
                checker1 = orig;
            }
        }
    }

    if (checker1 == SquareNone)
        last_move_ = 0;
    else if (checker2 == SquareNone) {
        if (last_move_) {
            if (Piece::is_pawn(square(checker1)))
                last_move_ = last_move_ | Gen::Move::DirectCheckFlag;
            else
                last_move_ = last_move_ | Gen::Move::RevealCheckFlag;
        }
        else
            last_move_ = Gen::Move(checker1, checker1) | Gen::Move::DirectCheckFlag; 
    }
    else {
        if (Piece::is_slider(square(checker1)))
            swap(checker1, checker2);

        // TODO: validate logic
        if (Piece::is_slider(square(checker1)))
            assert(false);

        last_move_ = Gen::Move(checker2 + Gen::delta_inc(checker2, king), checker1);
        last_move_ = last_move_ | Gen::Move::DirectCheckFlag | Gen::Move::RevealCheckFlag;
    }

    // key

    key_ ^= Hash::hash_castle(flags_);
    key_ ^= ep_is_valid() ? Hash::hash_ep(ep_sq_) : 0;
    key_ ^= Hash::hash_side(side_);

    assert(is_ok() == 0);
}

string Position::to_fen() const
{
    ostringstream oss;
    
    for (int rank = 7; rank >= 0; rank--) {
        int empty = 0;

        for (int file = 0; file < 8; file++) {
            Piece::Piece256 piece256 = square(to_sq88(file, rank));

            if (piece256 == Piece::PieceNone256)
                empty++;
            else {
                if (empty > 0) {
                    oss << to_string(empty);
                    empty = 0;
                }

                oss << Piece::to_char(piece256);
            }
        }

        if (empty > 0)
            oss << to_string(empty);

        if (rank > 0)
            oss << '/';
    }

    oss << ' ';

    oss << (side_ == Piece::White ? 'w' : 'b');

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

// TODO: optimize
void Position::note_move(Gen::Move& move) const
{
    int mside = side_;
    int oside = Piece::flip_side(side_);

    int king = king_sq(oside);

    int orig = move.orig();
    int dest = move.dest();

    Piece::Piece256 mflag = Piece::WhiteFlag256 << mside;

    Piece::Piece256 mpawn = Piece::make_pawn(mside);
    //Piece::Piece256 opawn = Piece::make_pawn(oside);

    Piece::Piece256 type256;
    Piece::Piece256 piece256;

    int inc;
    int sq;

    // direct check only

    if (move.is_castle()) {
        int rook_dest = dest > orig ? orig + 1 : orig - 1;

        if (Gen::delta_type(rook_dest, king) & Piece::RookFlag256) {
            inc = Gen::delta_inc(rook_dest, king);
            sq = rook_dest;

            do { sq += inc; } while (square(sq) == Piece::PieceNone256);

            if (sq == king)
                move.set_dir_check();
        }

        return;
    }

    if (move.is_ep()) {
        int cap = dest - pawn_inc(mside);

        // direct check?

        type256 = Gen::delta_type(dest, king);

        if (type256 & mpawn)
            move.set_dir_check();

        inc = dest - orig;

        // revealed checks impossible if ep captures towards king on its diagonal
        
        if (inc == Gen::delta_inc(dest, king))
            return;

        int checkers = 0;

        // revealed check on line of moving pawn

        type256 = Gen::delta_type(king, orig) & Piece::QueenFlags256;

        if (type256) {
            inc = Gen::delta_inc(king, orig);
            sq = king;
            piece256 = Piece::PieceNone256;

            do { 
                sq += inc;
            } while ( sq != dest
                  && (sq == cap || sq == orig || (piece256 = square(sq)) == Piece::PieceNone256));

            checkers += (piece256 & mflag) && (piece256 & type256);
        }

        if ((Gen::delta_type(king, cap) & Piece::QueenFlags256) == type256)
            goto ep_hack;

        // revealed check on line of captured pawn?

        type256 = Gen::delta_type(king, cap) & Piece::QueenFlags256;

        if (type256) { 
            inc = Gen::delta_inc(king, cap);
            sq = king;
            piece256 = Piece::PieceNone256;

            do {
                sq += inc;
            } while ( sq != dest
                  && (sq == cap || sq == orig || (piece256 = square(sq)) == Piece::PieceNone256));
        
            checkers += (piece256 & mflag) && (piece256 & type256);
        }

ep_hack:

        if (checkers >= 1) move.set_rev_check();
        if (checkers >= 2) move.set_rev_rev_check();

        return;
    }

    if (move.is_promo()) {
        int piece = move.promo_piece();

        assert(Piece::piece_is_ok(piece));

        Piece::Piece256 promo256 = Piece::to_piece256(mside, piece);
        
        assert(Piece::piece256_is_ok(promo256));

        // direct check?

        if (Gen::delta_type(dest, king) & promo256) {
            if (!Piece::is_slider(promo256))
                move.set_dir_check();
            else {
                inc = Gen::delta_inc(dest, king);
                sq = dest;

                do {
                    sq += inc;
                } while (sq == orig || square(sq) ==  Piece::PieceNone256);

                if (sq == king)
                    move.set_dir_check();
            }
        }

        goto revealed_check;
    }

    // direct check?

    piece256 = square(orig);

    assert(Piece::piece256_is_ok(piece256));

    type256 = Gen::delta_type(dest, king);

    if (piece256 & type256) {
        if (!Piece::is_slider(piece256))
            move.set_dir_check();
        else {
            inc = Gen::delta_inc(dest, king);
            sq = dest;

            do { sq += inc; } while (square(sq) == Piece::PieceNone256);

            if (sq == king)
                move.set_dir_check();
        }
    }

revealed_check:

    type256 = Gen::delta_type(orig, king) & Piece::QueenFlags256;

    if (!type256)
        return;

    int inc_orig = Gen::delta_inc(king, orig);
    int inc_dest = Gen::delta_inc(king, dest);

    if (inc_orig == inc_dest)
        return;

    sq = king;

    do { sq += inc_orig; } while ((piece256 = square(sq)) == Piece::PieceNone256);

    if (sq != orig)
        return;

    do { sq += inc_orig; } while ((piece256 = square(sq)) == Piece::PieceNone256);

    if (piece256 & mflag) {
        if (type256 & piece256) {
            //assert(false);

            //throw;

            move.set_rev_check();
        }
    }
}

void Position::make_move(const Gen::Move& move, Gen::Undo& undo)
{
    undo.flags          = flags_;
    undo.ep_sq          = ep_sq_;
    undo.half_moves     = half_moves_;
    undo.full_moves     = full_moves_;
    undo.key            = key_;
    undo.last_move      = last_move_;

    key_ ^= Hash::hash_castle(flags_);
    key_ ^= ep_is_valid() ? Hash::hash_ep(ep_sq_) : 0;
    key_ ^= Hash::hash_side(side_);

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

            flags_ &= Gen::castle_flag(dest);
        }
        
        if (move.is_promo()) {
            rem_piece(orig, true);
            add_piece(dest, Piece::to_piece256(side_, move.promo_piece()), true);
        }
        else
            mov_piece(orig, dest, true);
    }
        
    flags_ &= Gen::castle_flag(orig);
    ep_sq_ = move.is_double() ? dest - inc : SquareNone;
    half_moves_  = Piece::is_pawn(piece256) || move.is_capture() ? 0 : half_moves_ + 1;
    full_moves_ += side_;
    
    side_ = Piece::flip_side(side_);

    last_move_ = move;

    key_ ^= Hash::hash_castle(flags_);
    key_ ^= ep_is_valid() ? Hash::hash_ep(ep_sq_) : 0;
    key_ ^= Hash::hash_side(side_);

    assert(is_ok(false) == 0);
}

void Position::unmake_move(const Gen::Move& move, const Gen::Undo& undo)
{
    flags_          = undo.flags;
    ep_sq_          = undo.ep_sq;
    half_moves_     = undo.half_moves;
    full_moves_     = undo.full_moves;
    key_            = undo.key;
    last_move_      = undo.last_move;

    int orig = move.orig();
    int dest = move.dest();

    int piece256 = square(dest);
        
    int inc = pawn_inc(Piece::flip_side(side_));

    if (move.is_castle()) {
        int rook_orig = dest > orig ? orig + 3 : orig - 4;
        int rook_dest = dest > orig ? orig + 1 : orig - 1;
        Piece::Piece256 rook256 = square(rook_dest);

        rem_piece(dest, false);
        rem_piece(rook_dest, false);

        add_piece(orig, piece256, false);
        add_piece(rook_orig, rook256, false);
    }
    else if (move.is_ep()) {
        rem_piece(dest, false);
        add_piece(orig, piece256, false);
        add_piece(dest - inc, Piece::to_piece256(side_, Piece::Pawn), false);
    }
    else {
        rem_piece(dest, false);

        if (move.is_promo())
            add_piece(orig, Piece::to_piece256(Piece::flip_side(side_), Piece::Pawn), false);
        else
            add_piece(orig, piece256, false);
        
        if (move.is_capture())
            add_piece(dest, move.capture_piece(), false);
    }

    side_ = Piece::flip_side(side_);

    assert(is_ok() == 0);
}

int Position::is_ok(bool in_check) const
{
    if (!Piece::side_is_ok(side_))
        return __LINE__;
        
    if (piece_list_[Piece::WhiteKing12].size() != 1) return __LINE__;
    if (piece_list_[Piece::BlackKing12].size() != 1) return __LINE__;
    
    size_t type_min[6] = { 0,  0,  0,  0, 0, 1 };
    size_t type_max[6] = { 8, 10, 10, 10, 9, 1 };

    size_t sq_count[128] = { };

    for (int side : { Piece::White, Piece::Black }) {
        size_t color_count = 0;

        for (int piece = Piece::Pawn; piece <= Piece::King; piece++) {
            size_t type_count = 0;

            const int p12 = Piece::to_piece12(side, piece);

            for (const int sq : piece_list(p12)) {
                if (!is_sq88(sq))
                    return __LINE__;

                Piece::Piece256 piece256 = Piece::to_piece256(side, piece);

                if (!Piece::piece256_is_ok(piece256))
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
        int board_count = square(sq) != Piece::PieceNone256;

        if (plist_count != board_count)
            return __LINE__;
    }

    if (in_check && side_attacks(side_, king_sq(Piece::flip_side(side_))))
        return __LINE__;

    if (flags_ < 0 && flags_ >= 16)
        return __LINE__;

    if (ep_sq_ != SquareNone) {
        int rank = sq88_rank(ep_sq_, Piece::flip_side(side_));

        if (rank != Rank3)
            return __LINE__;

        Piece::Piece256 opawn = Piece::make_pawn(Piece::flip_side(side_));

        int inc = pawn_inc(Piece::flip_side(side_));

        if (square(ep_sq_ + inc) != opawn)
            return __LINE__;
    }

    //if (calc_key() != key_) return __LINE__;

    // int flags_ = 0;
    // int ep_sq_ = SquareNone;
    // Gen::Move last_move_;
    
    return 0;
}

void Position::add_piece(int sq, Piece::Piece256 piece256, bool update_key)
{
    assert(is_sq88(sq));
    assert(is_empty(sq));
    assert(Piece::piece256_is_ok(piece256));

    const int p12 = Piece::to_piece12(piece256);

    assert(Piece::piece12_is_ok(p12));

    piece_list_[p12].add(sq);

    if (update_key)
        key_ ^= Hash::hash_piece(p12, sq);

    square(sq) = piece256;
}

void Position::rem_piece(int sq, bool update_key)
{
    assert(is_sq88(sq));
    assert(!is_empty(sq));

    Piece::Piece256 piece256 = square(sq);
    
    assert(Piece::piece256_is_ok(piece256));

    const int p12 = Piece::to_piece12(piece256);
    
    assert(Piece::piece12_is_ok(p12));
    
    piece_list_[p12].remove(sq);
   
    if (update_key)
        key_ ^= Hash::hash_piece(p12, sq);

    square(sq) = Piece::PieceNone256;
}

void Position::mov_piece(int orig, int dest, bool update_key)
{
    assert(is_sq88(orig));
    assert(is_sq88(dest));
    assert(!is_empty(orig));
    assert(is_empty(dest));

    Piece::Piece256 piece256 = square(orig);

    assert(Piece::piece256_is_ok(piece256));

    const int p12 = Piece::to_piece12(piece256);

    assert(Piece::piece12_is_ok(p12));
    
    piece_list_[p12].replace(orig, dest);

    if (update_key) {
        key_ ^= Hash::hash_piece(p12, orig);
        key_ ^= Hash::hash_piece(p12, dest);
    }

    swap(square(orig), square(dest));
}

bool Position::side_attacks(int side, int dest) const
{
    assert(Piece::side_is_ok(side));
    assert(is_sq88(dest));

    int king = king_sq(side);

    if (Gen::delta_type(king, dest) & Piece::KingFlag256)
        return true;

    Piece::Piece256 pawn256 = Piece::WhitePawn256 << side;

    {
        int inc = pawn_inc(side);

        if (int orig = dest - inc - 1; square(orig) == pawn256) return true;
        if (int orig = dest - inc + 1; square(orig) == pawn256) return true;
    }

    for (const int orig : piece_list(Piece::WhiteKnight12 + side)) {
        if (Gen::delta_type(orig, dest) & Piece::KnightFlag256)
            return true;
    }

    for (const int orig : piece_list(Piece::WhiteBishop12 + side)) {
        if (!(Gen::delta_type(orig, dest) & Piece::BishopFlag256))
            continue;

        int inc = Gen::delta_inc(dest, orig);
        int sq = dest;

        do { sq += inc; } while (square(sq) == Piece::PieceNone256);

        if (sq == orig)
            return true;
    }

    for (const int orig : piece_list(Piece::WhiteRook12 + side)) {
        if (!(Gen::delta_type(orig, dest) & Piece::RookFlag256))
            continue;

        int inc = Gen::delta_inc(dest, orig);
        int sq = dest;

        do { sq += inc; } while (square(sq) == Piece::PieceNone256);

        if (sq == orig)
            return true;
    }
    
    for (const int orig : piece_list(Piece::WhiteQueen12 + side)) {
        if (!(Gen::delta_type(orig, dest) & Piece::QueenFlags256))
            continue;

        int inc = Gen::delta_inc(dest, orig);
        int sq = dest;

        do { sq += inc; } while (square(sq) == Piece::PieceNone256);

        if (sq == orig)
            return true;
    }

    return false;
}

bool Position::piece_attacks(int orig, int dest) const
{
    assert(is_sq88(orig));
    assert(is_sq88(dest));
    assert(!is_empty(orig));

    Piece::Piece256 piece256 = square(orig);

    if (!(Gen::delta_type(orig, dest) & piece256))
        return false;

    if (!Piece::is_slider(piece256))
        return true;

    int inc = Gen::delta_inc(dest, orig);
    int sq = dest;

    do { sq += inc; } while (square(sq) == Piece::PieceNone256);

    return sq == orig;
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
            Piece::Piece256 piece256 = square(to_sq88(file, rank));

            if (Piece::is_white(piece256))
                oss << '-' << Piece::to_char(Piece::to_piece(piece256)) << '-';
            else if (Piece::is_black(piece256))
                oss << '<' << Piece::to_char(Piece::to_piece(piece256)) << '>';
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

        Piece::Piece256 piece256 = square(sq);

        if (piece256 == Piece::PieceNone256)
            continue;

        assert(Piece::piece256_is_ok(piece256));

        int p12 = Piece::to_piece12(piece256);

        key ^= Hash::hash_piece(p12, sq);
    }

    key ^= Hash::hash_castle(flags_);
    key ^= ep_is_valid() ? Hash::hash_ep(ep_sq_) : 0;
    key ^= Hash::hash_side(side_);

    return key;
}

bool Position::ep_is_valid() const
{
    if (ep_sq_ == SquareNone)
        return false;

    int sq = ep_sq_ - pawn_inc(side_);

    Piece::Piece256 mpawn = Piece::make_pawn(side_);

    return square(sq - 1) == mpawn || square(sq + 1) == mpawn;
}

void Position::mark_pins(bitset<128>& pins) const
{
    assert(pins.none());

    const int king = king_sq();

    const int mside = side_;
    const int oside = Piece::flip_side(side_);

    const Piece::Piece256 mflag = Piece::make_flag(mside);

    for (int p12 = Piece::WhiteBishop12 + oside; p12 <= Piece::BlackQueen12; p12 += 2) {
        for (const int orig : piece_list_[p12]) {
            assert(is_sq88(orig));

            if (!(square(orig) & Gen::delta_type(king, orig)))
                continue;

            const int inc = Gen::delta_inc(king, orig);
            int sq1 = king;
            int sq2 = orig;

            do { sq1 += inc; } while (square(sq1) == Piece::PieceNone256);
            
            assert(is_sq88(sq1));

            if (!(square(sq1) & mflag))
                continue;

            do { sq2 -= inc; } while (square(sq2) == Piece::PieceNone256);

            assert(is_sq88(sq2));

            if (sq1 != sq2)
                continue;

            assert(!pins.test(sq1));

            pins.set(sq1);
        }
    }
}

