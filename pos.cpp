#include <algorithm>
#include <array>
#include <iostream>
#include <sstream>
#include <cassert>
#include <cstring>
#include "gen.h"
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
            add_piece(to_sq88(file, rank), Piece::to_piece256(c));
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

        // TODO: or throw?
        assert(is_sq88(ep_sq_));
    }

    // half moves counter

    if (half_moves != "-")
        half_moves_ = stoi(half_moves);

    // full moves counter
    
    if (full_moves != "-")
        full_moves_ = stoi(full_moves);

    // determine if we're in check and from which squares

    assert(is_ok() == 0);
}

string Position::get_fen() const
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

void Position::make_move(const Gen::Move& move, Gen::Undo& undo)
{
    undo.flags          = flags_;
    undo.ep_sq          = ep_sq_;
    undo.half_moves     = half_moves_;
    undo.full_moves     = full_moves_;
    undo.last_move      = last_move_;

    int orig = move.orig();
    int dest = move.dest();

    int piece256 = square(orig);
        
    int inc = pawn_inc(side_);

    if (move.is_castle()) {
        int rook_orig = dest > orig ? orig + 3 : orig - 4;
        int rook_dest = dest > orig ? orig + 1 : orig - 1;

        move_piece(orig, dest);
        move_piece(rook_orig, rook_dest);
    }
    else if (move.is_ep()) {
        rem_piece(dest - inc);
        move_piece(orig, dest);
    }
    else {
        if (move.is_capture()) {
            rem_piece(dest);

            flags_ &= Gen::castle_flag(dest);
        }
        
        if (move.is_promo()) {
            rem_piece(orig);
            add_piece(dest, Piece::to_piece256(side_, move.promo_piece()));
        }
        else
            move_piece(orig, dest);
    }
        
    flags_ &= Gen::castle_flag(orig);
    ep_sq_ = move.is_double() ? dest - inc : SquareNone;
    half_moves_  = Piece::is_pawn(piece256) || move.is_capture() ? 0 : half_moves_ + 1;
    full_moves_ += side_;
    
    side_ = Piece::flip_side(side_);

    last_move_ = move;

    assert(is_ok(false) == 0);
}

void Position::unmake_move(const Gen::Move& move, const Gen::Undo& undo)
{
    flags_          = undo.flags;
    ep_sq_          = undo.ep_sq;
    half_moves_     = undo.half_moves;
    full_moves_     = undo.full_moves;
    last_move_      = undo.last_move;

    int orig = move.orig();
    int dest = move.dest();

    int piece256 = square(dest);
        
    int inc = pawn_inc(Piece::flip_side(side_));

    if (move.is_castle()) {
        int rook_orig = dest > orig ? orig + 3 : orig - 4;
        int rook_dest = dest > orig ? orig + 1 : orig - 1;
        Piece::Piece256 rook256 = square(rook_dest);

        rem_piece(dest);
        rem_piece(rook_dest);

        add_piece(orig, piece256);
        add_piece(rook_orig, rook256);
    }
    else if (move.is_ep()) {
        rem_piece(dest);
        add_piece(orig, piece256);
        add_piece(dest - inc, Piece::to_piece256(side_, Piece::Pawn));
    }
    else {
        rem_piece(dest);

        if (move.is_promo())
            add_piece(orig, Piece::to_piece256(Piece::flip_side(side_), Piece::Pawn));
        else
            add_piece(orig, piece256);
        
        if (move.is_capture())
            add_piece(dest, move.capture_piece());
    }

    side_ = Piece::flip_side(side_);

    assert(is_ok() == 0);
}

int Position::is_ok(bool incheck) const
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

            int p12 = Piece::to_piece12(side, piece);

            for (int sq : piece_list(p12)) {
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

    if (incheck && side_attacks(side_, king_sq(Piece::flip_side(side_))))
        return __LINE__;

    // int inc = pawn_inc(Piece::flip_side(side_));
    // int flags_ = 0;
    // int ep_sq_ = SquareNone;
    // Gen::Move last_move_;
    
    return 0;
}

void Position::add_piece(int sq, Piece::Piece256 piece256)
{
    assert(is_sq88(sq));
    assert(is_empty(sq));
    assert(Piece::piece256_is_ok(piece256));

    int p12 = Piece::to_piece12(piece256);

    assert(Piece::piece12_is_ok(p12));

    piece_list_[p12].add(sq);

    square(sq) = piece256;
}

void Position::rem_piece(int sq)
{
    assert(is_sq88(sq));
    assert(!is_empty(sq));

    Piece::Piece256 piece256 = square(sq);
    
    assert(Piece::piece256_is_ok(piece256));

    int p12 = Piece::to_piece12(piece256);
    
    assert(Piece::piece12_is_ok(p12));
    
    piece_list_[p12].remove(sq);

    square(sq) = Piece::PieceNone256;
}

void Position::move_piece(int orig, int dest)
{
    assert(is_sq88(orig));
    assert(is_sq88(dest));
    assert(!is_empty(orig));
    assert(is_empty(dest));

    Piece::Piece256 piece256 = square(orig);
    
    assert(Piece::piece256_is_ok(piece256));

    int p12 = Piece::to_piece12(piece256);
    
    assert(Piece::piece12_is_ok(p12));
    
    piece_list_[p12].replace(orig, dest);

    swap(square(orig), square(dest));
}

bool Position::side_attacks(int side, int dest) const
{
    assert(Piece::side_is_ok(side));
    assert(is_sq88(dest));

    int king = king_sq(side);

    if (Gen::move_flag(king, dest) & Piece::KingFlag256)
        return true;

    Piece::Piece256 pawn256 = Piece::WhitePawn256 << side;
   
    int inc = pawn_inc(side);

    if (int orig = dest - inc - 1; square(orig) == pawn256) return true;
    if (int orig = dest - inc + 1; square(orig) == pawn256) return true;

    for (int orig : piece_list(Piece::WhiteKnight12 + side)) {
        if (Gen::move_flag(orig, dest) & Piece::KnightFlag256)
            return true;
    }

    for (int orig : piece_list(Piece::WhiteBishop12 + side)) {
        if (Gen::move_flag(orig, dest) & Piece::BishopFlag256) {
            int dir = Gen::move_dir(dest, orig);
            int sq = dest;

            do { sq += dir; } while (square(sq) == Piece::PieceNone256);

            if (sq == orig)
                return true;
        }
    }

    for (int orig : piece_list(Piece::WhiteRook12 + side)) {
        if (Gen::move_flag(orig, dest) & Piece::RookFlag256) {
            int dir = Gen::move_dir(dest, orig);
            int sq = dest;

            do { sq += dir; } while (square(sq) == Piece::PieceNone256);

            if (sq == orig)
                return true;
        }
    }
    
    for (int orig : piece_list(Piece::WhiteQueen12 + side)) {
        if (Gen::move_flag(orig, dest) & Piece::QueenFlags256) {
            int dir = Gen::move_dir(dest, orig);
            int sq = dest;

            do { sq += dir; } while (square(sq) == Piece::PieceNone256);

            if (sq == orig)
                return true;
        }
    }

    return false;
}

