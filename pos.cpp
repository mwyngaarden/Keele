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
    for (int i = 0; i < 256; i++)
        square_[i] = Piece::PieceInvalid256;

    for (int i = 0; i < 64; i++)
        square(to_sq88(i)) = Piece::PieceNone256;

    check_sq_[0] = SquareNone;
    check_sq_[1] = SquareNone;
    
    for (int i = 0; i < 2; i++) {
        //piece_all_count_[i] = 0;

        for (int j = 0; j < 6; j++) {
            piece_type_count_[i][j] = 0;

            for (int k = 0; k < 11; k++)
                piece_type_pos_[i][j][k] = SquareNone;
        }

        //for (int j = 0; j < 17; j++)
            //piece_all_pos_[i][j] = SquareNone;
    }
}

Position::Position(const string& fen)
{
    init();

    Tokenizer fields(fen, ' ');

    const string& pieces        = fields[0];
    const string& side          = fields[1];
    const string& flags         = fields[2];
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

    // set_checks();

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
    undo.check_sq[0]    = check_sq_[0];
    undo.check_sq[1]    = check_sq_[1];
    undo.last_move      = last_move_;

    int orig = move.orig();
    int dest = move.dest();

    int piece256 = square(orig);
        
    int inc = pawn_inc(side_);

    if (move.is_castle()) {
        int rook_orig = dest > orig ? orig + 3 : orig - 4;
        int rook_dest = dest > orig ? orig + 1 : orig - 1;
        Piece::Piece256 rook256 = square(rook_orig);

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
    check_sq_[0]    = undo.check_sq[0];
    check_sq_[1]    = undo.check_sq[1];
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
        if (move.is_promo())
            add_piece(orig, Piece::to_piece256(Piece::flip_side(side_), Piece::Pawn));
        else
            add_piece(orig, piece256);

        rem_piece(dest);
        
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
        
    if (piece_type_count_[Piece::White][Piece::King] != 1) return __LINE__;
    if (piece_type_count_[Piece::Black][Piece::King] != 1) return __LINE__;

    if (!is_sq88(piece_type_pos_[Piece::White][Piece::King][0])) return __LINE__;
    if (!is_sq88(piece_type_pos_[Piece::Black][Piece::King][0])) return __LINE__;
    
    int type_min[6] = { 0,  0,  0,  0, 0, 1 };
    int type_max[6] = { 8, 10, 10, 10, 9, 1 };

    int sq_count[128] = { };

    for (int side : { Piece::White, Piece::Black }) {
        int all_count = 0;

        for (int piece = Piece::Pawn; piece <= Piece::King; piece++) {
            int type_count = 0;
            int sq;

            for (const int* plist = get_plist(side, piece); (sq = *plist) != SquareNone; plist++) {
                if (!is_sq88(sq))
                    return __LINE__;

                Piece::Piece256 piece256 = Piece::to_piece256(side, piece);

                if (!Piece::piece256_is_ok(piece256))
                    return __LINE__;
                
                if (square(sq) != piece256)
                    return __LINE__;

                sq_count[sq]++;

                all_count++;
                type_count++;
            }

            if (type_count != piece_type_count_[side][piece])
                return __LINE__;

            if (type_count < type_min[piece]) return __LINE__;
            if (type_count > type_max[piece]) return __LINE__;
        }

        //if (all_count != piece_all_count_[side])
            //return __LINE__;

        if (all_count > 16)
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
    // int check_sq_[2];
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

    int side = p12 % 2;
    int piece = p12 / 2;
    
    {
        int& count = piece_type_count_[side][piece];

        piece_type_pos_[side][piece][count++] = sq;
    }

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
    
    int side = p12 % 2;
    int piece = p12 / 2;

    {
        int* list = piece_type_pos_[side][piece];
        
        int& count = piece_type_count_[side][piece];

        while (*list != sq) list++;

        count--;

        *list = piece_type_pos_[side][piece][count];
        piece_type_pos_[side][piece][count] = SquareNone;
    }

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
    
    int side = p12 & 1;
    int piece = p12 >> 1;

    {
        int* list = piece_type_pos_[side][piece];
        
        while (*list != orig) list++;

        *list = dest;
    }

    swap(square(orig), square(dest));
}

string Position::dump() const
{
    ostringstream oss;

#if 0
    for (int rank = 7; rank >= 0; rank--) {
        if (rank < 7)
            oss << endl;

        for (int file = 0; file < 8; file++) {
            if (file > 0)
                oss << "  ";

            Piece::Piece256 piece256 = square(to_sq88(file, rank));

            if (piece256 != Piece::None256)
                oss << Piece::to_char(piece256);
            else
                oss << '.';
        }
    }
#endif

    for (int i = 0; i < 2; i++) {
        if (i > 0)
            oss << endl;

        for (int j = 0; j < 6; j++) {
            if (j > 0)
                oss << endl;

            for (int k = 0; k < 11; k++) {
                if (k > 0)
                    oss << ' ';

                oss << piece_type_pos_[i][j][k];
            }
        }
    }

    return oss.str();
}

#if 0
void Position::set_checks()
{
    check_sq_[0] = SquareNone;
    check_sq_[1] = SquareNone;

    int king = king_sq();

    assert(is_sq88(king));

    int sq;

    // TODO: early termination when two checks found?
    for (const int* plist = get_plist(Piece::flip_side(side_)); (sq = *plist) != SquareNone; plist++) {
        assert(is_sq88(sq));

        Piece::Piece256 piece256 = square(sq);

        assert(Piece::piece256_is_ok(piece256));

        if (!(Gen::move_flag(sq, king) & piece256))
            continue;

        assert(!Piece::is_king(piece256));

        if (Piece::is_slider(piece256)) {
            int dir = Gen::move_dir(king, sq);
            int tmp = sq + dir;

            while (square(tmp) == Piece::PieceNone256) tmp += dir;

            if (tmp == sq)
                add_check(sq);
        }
        else 
            add_check(sq);
    }
}
#endif

void Position::set_checks(const Gen::Move& last_move)
{
    check_sq_[0] = SquareNone;
    check_sq_[1] = SquareNone;

    int orig = last_move.orig();
    int dest = last_move.orig();

    int king = king_sq();
    
    assert(is_sq88(king));

    // only single direct check possible
    
    if (last_move.is_castle()) {
        int rook = dest > orig ? dest - 1 : dest + 1;

        if (Gen::move_flag(king, rook) & Piece::RookFlag256) {
            int dir = Gen::move_dir(king, rook);
            int sq = king + dir;

            while (square(sq) == Piece::PieceNone256) sq += dir;

            if (sq == rook)
                add_check(rook);
        }

        return;
    }

    Piece::Piece256 move_flag;

    // revealed check
    
    if (move_flag = Gen::move_flag(king, orig); Piece::is_slider(move_flag)) {
        int dir = Gen::move_dir(king, orig);
        int sq = king + dir;

        while (square(sq) == Piece::PieceNone256) sq += dir;

        if (is_op(sq) && (square(sq) & move_flag))
            add_check(sq);
    }

    // direct check
    
    int piece256 = square(dest);

    assert(!Piece::is_king(piece256));

    if (Gen::move_flag(king, dest) & piece256) {
        if (Piece::is_slider(piece256)) {
            int dir = Gen::move_dir(king, dest);
            int sq = king + dir;

            while (square(sq) == Piece::PieceNone256) sq += dir;

            if (sq == dest)
                add_check(sq);
        }
        else 
            add_check(dest);
    }
}

#if 0
bool Position::in_check(int side) const
{
    assert(Piece::side_is_ok(side));

    int king = king_sq(side);
    
    assert(is_sq88(king));

    int sq;

    for (const int* plist = get_plist(Piece::flip_side(side)); (sq = *plist) != SquareNone; plist++) {
        assert(is_sq88(sq));

        Piece::Piece256 piece256 = square(sq);

        assert(Piece::piece256_is_ok(piece256));

        if (Gen::move_flag(sq, king) & piece256) {
            assert(!Piece::is_king(piece256));

            if (Piece::is_slider(piece256)) {
                int dir = Gen::move_dir(king, sq);
                int tmp = sq + dir;

                while (square(tmp) == Piece::PieceNone256) tmp += dir;

                return tmp == sq;
            }
            else
                return true;
        }
    }

    return false;
}
#endif

bool Position::side_attacks(int side, int dest) const
{
    assert(Piece::side_is_ok(side));
    assert(is_sq88(dest));

    int king = king_sq(side);

    if (Gen::move_flag(king, dest) & Piece::KingFlag256)
        return true;

    int inc = pawn_inc(side);

    Piece::Piece256 pawn256 = Piece::WhitePawn256 << side;

    if (square(dest - inc - 1) == pawn256) return true;
    if (square(dest - inc + 1) == pawn256) return true;

    for (int i = 0; i < piece_type_count_[side][Piece::Knight]; i++) {
        int orig = piece_type_pos_[side][Piece::Knight][i];

    //for (const int* plist = get_plist(side, Piece::Knight); *plist != SquareNone; plist++) {
        //int orig = *plist;

        if (Gen::move_flag(orig, dest) & Piece::KnightFlag256)
            return true;
    }

    for (int i = 0; i < piece_type_count_[side][Piece::Bishop]; i++) {
        int orig = piece_type_pos_[side][Piece::Bishop][i];

    //for (const int* plist = get_plist(side, Piece::Bishop); *plist != SquareNone; plist++) {
        //int orig = *plist;

        if (Gen::move_flag(orig, dest) & Piece::BishopFlag256) {
            int dir = Gen::move_dir(dest, orig);
            int sq = dest;

            do { sq += dir; } while (square(sq) == Piece::PieceNone256);

            if (sq == orig) return true;
        }
    }

    for (int i = 0; i < piece_type_count_[side][Piece::Rook]; i++) {
        int orig = piece_type_pos_[side][Piece::Rook][i];

    //for (const int* plist = get_plist(side, Piece::Rook); *plist != SquareNone; plist++) {
        //int orig = *plist;

        if (Gen::move_flag(orig, dest) & Piece::RookFlag256) {
            int dir = Gen::move_dir(dest, orig);
            int sq = dest;

            do { sq += dir; } while (square(sq) == Piece::PieceNone256);

            if (sq == orig) return true;
        }
    }
    
    for (int i = 0; i < piece_type_count_[side][Piece::Queen]; i++) {
        int orig = piece_type_pos_[side][Piece::Queen][i];

    //for (const int* plist = get_plist(side, Piece::Queen); *plist != SquareNone; plist++) {
        //int orig = *plist;

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






