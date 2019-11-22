#include <algorithm>
#include <array>
#include <bitset>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <utility>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include "gen.h"
#include "hash.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
#include "string.h"
#include "types.h"
using namespace std;

static constexpr bool UpdateInfo = false;

Position::Position(const string& fen)
{
    // init

    for (int i = 0; i < 192; i++)
        square_[i] = PieceInvalid256;

    for (int i = 0; i < 64; i++)
        square(to_sq88(i)) = PieceNone256;

    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 10; j++)
            pawn_file_[i][j] = 0;

    // fen

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
            add_piece(to_sq88(file, rank), char_to_piece256(c), UpdateInfo);
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
        int sq = san_to_sq88(ep_sq);
        
        assert(sq88_is_ok(sq));

        ep_sq_ = ep_is_valid(sq) ? sq : SquareNone;
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

    if (UpdateInfo) {
        key_ ^= hash_castle(flags_);
        key_ ^= ep_sq_ != SquareNone ? hash_ep(ep_sq_) : 0;
        key_ ^= side_ == White ? hash_side() : 0;
    }

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

    if (UpdateInfo) {
        key_ ^= hash_castle(flags_);
        key_ ^= ep_sq_ != SquareNone ? hash_ep(ep_sq_) : 0;
        key_ ^= hash_side();
    }

    const int orig = move.orig();
    const int dest = move.dest();

    const int piece256 = square(orig);
        
    const int inc = pawn_inc(side_);

    if (move.is_castle()) {
        int rorig = dest > orig ? orig + 3 : orig - 4;
        int rdest = dest > orig ? orig + 1 : orig - 1;

        mov_piece( orig,  dest, UpdateInfo);
        mov_piece(rorig, rdest, UpdateInfo);
    }
    else if (move.is_ep()) {
        assert(dest == ep_sq_);

        rem_piece(dest - inc, UpdateInfo);
        mov_piece(orig, dest, UpdateInfo);
    } 
    else {
        if (move.is_capture())
            rem_piece(dest, UpdateInfo);

        if (move.is_promo()) {
            rem_piece(orig, UpdateInfo);
            add_piece(dest, to_piece256(side_, move.promo_piece()), UpdateInfo);
        }
        else
            mov_piece(orig, dest, UpdateInfo);
    }
       
    flags_ &= castle_flag(orig) & castle_flag(dest);
    ep_sq_ = move.is_double() && ep_is_valid(ep_dual(dest)) ? ep_dual(dest) : SquareNone;
    half_moves_  = is_pawn(piece256) || move.is_capture() ? 0 : half_moves_ + 1;
    full_moves_ += side_;
    
    side_ = flip_side(side_);

    // update checkers

    checkers_count_ = 0;
    
    set_checkers_fast(move);

    // update key, pst scores, etc.

    if (UpdateInfo) {
        key_ ^= hash_castle(flags_);
        key_ ^= ep_sq_ != SquareNone ? hash_ep(ep_sq_) : 0;
    }
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
        int rorig = dest > orig ? orig + 3 : orig - 4;
        int rdest = dest > orig ? orig + 1 : orig - 1;

        mov_piece( dest,  orig);
        mov_piece(rdest, rorig);
    }
    else if (move.is_ep()) {
        mov_piece(dest, orig);
        add_piece(ep_dual(dest), make_pawn(side_));
    }
    else {
        if (move.is_promo()) {
            rem_piece(dest);
            add_piece(orig, make_pawn(flip_side(side_)));
        }
        else
            mov_piece(dest, orig);

        if (move.is_capture())
            add_piece(dest, move.capture_piece());
    }

    side_ = flip_side(side_);
}

int Position::is_ok(bool check_test) const
{
    return 0;

    if (!side_is_ok(side_)) return __LINE__;

    if (piece_list_[WK12].size() != 1) return __LINE__;
    if (piece_list_[BK12].size() != 1) return __LINE__;
    
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

    if (check_test && side_attacks(side_, king_sq(flip_side(side_))))
        return __LINE__;

    if (flags_ < 0 || flags_ >= 16)
        return __LINE__;

    if (ep_sq_ != SquareNone) {
        int rank = sq88_rank(ep_sq_, flip_side(side_));

        if (rank != Rank3)
            return __LINE__;

        u8 opawn = make_pawn(flip_side(side_));

        if (square(ep_dual(ep_sq_)) != opawn)
            return __LINE__;
    }

    return 0;
}

void Position::add_piece(int sq, u8 piece256, bool update)
{
    assert(sq88_is_ok(sq));
    assert(empty(sq));
    assert(piece256_is_ok(piece256));

    const int p12 = P256ToP12[piece256];

    assert(piece12_is_ok(p12));

    piece_list_[p12].add(sq);

    if (is_pawn(piece256)) {
        const int file = sq88_file(sq);
        const int rank = sq88_rank(sq);

        assert((pawn_file_[p12 & 1][1 + file] & (1 << rank)) == 0);

        pawn_file_[p12 & 1][1 + file] ^= 1 << rank;
    }

    if (update)
        key_ ^= hash_piece(p12, sq);

    square(sq) = piece256;
}

void Position::rem_piece(int sq, bool update)
{
    assert(sq88_is_ok(sq));
    assert(!empty(sq));

    const u8 piece256 = square(sq);
    
    assert(piece256_is_ok(piece256));

    const int p12 = P256ToP12[piece256];
    
    assert(piece12_is_ok(p12));
    
    piece_list_[p12].remove(sq);
    
    if (is_pawn(piece256)) {
        const int file = sq88_file(sq);
        const int rank = sq88_rank(sq);

        assert((pawn_file_[p12 & 1][1 + file] & (1 << rank)) != 0);

        pawn_file_[p12 & 1][1 + file] ^= 1 << rank;
    }

    if (update)
        key_ ^= hash_piece(p12, sq);

    square(sq) = PieceNone256;
}

void Position::mov_piece(int orig, int dest, bool update)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));
    assert(!empty(orig));
    assert(empty(dest));

    const u8 piece256 = square(orig);

    assert(piece256_is_ok(piece256));

    const int p12 = P256ToP12[piece256];

    assert(piece12_is_ok(p12));
    
    piece_list_[p12].replace(orig, dest);
    
    if (is_pawn(piece256)) {
        const int ofile = sq88_file(orig);
        const int orank = sq88_rank(orig);
        const int dfile = sq88_file(dest);
        const int drank = sq88_rank(dest);

        assert((pawn_file_[p12 & 1][1 + ofile] & (1 << orank)) != 0);
        assert((pawn_file_[p12 & 1][1 + dfile] & (1 << drank)) == 0);

        pawn_file_[p12 & 1][1 + ofile] ^= 1 << orank;
        pawn_file_[p12 & 1][1 + dfile] ^= 1 << drank;
    }

    if (update) {
        key_ ^= hash_piece(p12, orig);
        key_ ^= hash_piece(p12, dest);
    }

    swap(square(orig), square(dest));
}

bool Position::side_attacks(int side, int dest) const
{
    assert(side_is_ok(side));
    assert(sq88_is_ok(dest));

    if (pseudo_attack(king_sq(side), dest, KingFlag256))
        return true;

    const u8 pawn256 = make_pawn(side);

    const int pinc = pawn_inc(side);

    if (int orig = dest - pinc - 1; square(orig) == pawn256) return true;
    if (int orig = dest - pinc + 1; square(orig) == pawn256) return true;

    for (const int orig : piece_list(WN12 + side))
        if (pseudo_attack(orig, dest, KnightFlag256))
            return true;

    for (const auto orig : piece_list_[WB12 + side]) {
        if (!pseudo_attack(orig, dest, BishopFlag256)) continue;
        const int inc = delta_inc(dest, orig);
        int sq = dest + inc;
        while (square(sq) == PieceNone256) sq += inc;
        if (sq == orig) return true;
    }

    for (const auto orig : piece_list_[WR12 + side]) {
        if (!pseudo_attack(orig, dest, RookFlag256)) continue;
        const int inc = delta_inc(dest, orig);
        int sq = dest + inc;
        while (square(sq) == PieceNone256) sq += inc;
        if (sq == orig) return true;
    }

    for (const auto orig : piece_list_[WQ12 + side]) {
        if (!pseudo_attack(orig, dest, QueenFlags256)) continue;
        const int inc = delta_inc(dest, orig);
        int sq = dest + inc;
        while (square(sq) == PieceNone256) sq += inc;
        if (sq == orig) return true;
    }

    return false;
}

bool Position::piece_attacks(int orig, int dest) const
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));
    assert(!empty(orig));

    const u8 piece256 = square(orig);

    assert(piece256_is_ok(piece256));

    if (!pseudo_attack(orig, dest, piece256))
        return false;

    return !is_slider(piece256) || empty(orig, dest);
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
            const u8 piece256 = square(to_sq88(file, rank));

            if (is_white(piece256))
                oss << '-' << (char)        piece256_to_char(piece256)  << '-';
            else if (is_black(piece256))
                oss << '<' << (char)toupper(piece256_to_char(piece256)) << '>';
            else  {
                const int dark = ~(file ^ rank) & 1;

                oss << ' ' << (dark ? '.' : ' ') << ' ';
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

        int p12 = P256ToP12[piece256];

        assert(piece12_is_ok(p12));

        key ^= hash_piece(p12, sq);
    }

    key ^= hash_castle(flags_);
    key ^= ep_sq_ != SquareNone ? hash_ep(ep_sq_) : 0;
    key ^= side_ == White ? hash_side() : 0;

    return key;
}

bool Position::ep_is_valid(int sq) const
{
    assert(sq88_is_ok(sq));
    assert(sq88_rank(sq) == Rank3 || sq88_rank(sq) == Rank6);

    sq = ep_dual(sq);
    
    assert(is_pawn(square(sq)));

    const u8 opawn = flip_pawn(square(sq));

    return square(sq - 1) == opawn || square(sq + 1) == opawn;
}

void Position::mark_pins(BitSet& pins) const
{
    assert(pins.none());

    const int king = king_sq();

    const int mside =           side_;
    const int oside = flip_side(side_);

    const u8 mflag = make_flag(mside);

    u8 piece256;

    int sq1;
    int sq2;
    int inc;

    for (const auto orig : piece_list_[WB12 + oside]) {
        if (!pseudo_attack(orig, king, BishopFlag256)) continue;
        inc = delta_inc(king, orig);
        sq1 = king + inc;
        while ((piece256 = square(sq1)) == PieceNone256) sq1 += inc;
        if ((piece256 & mflag) == 0) continue;
        sq2 = orig - inc;
        while (square(sq2) == PieceNone256) sq2 -= inc;
        if (sq1 == sq2) pins.set(sq1);
    }

    for (const auto orig : piece_list_[WR12 + oside]) {
        if (!pseudo_attack(orig, king, RookFlag256)) continue;
        inc = delta_inc(king, orig);
        sq1 = king + inc;
        while ((piece256 = square(sq1)) == PieceNone256) sq1 += inc;
        if ((piece256 & mflag) == 0) continue;
        sq2 = orig - inc;
        while (square(sq2) == PieceNone256) sq2 -= inc;
        if (sq1 == sq2) pins.set(sq1);
    }

    for (const auto orig : piece_list_[WQ12 + oside]) {
        if (!pseudo_attack(orig, king, QueenFlags256)) continue;
        inc = delta_inc(king, orig);
        sq1 = king + inc;
        while ((piece256 = square(sq1)) == PieceNone256) sq1 += inc;
        if ((piece256 & mflag) == 0) continue;
        sq2 = orig - inc;
        while (square(sq2) == PieceNone256) sq2 -= inc;
        if (sq1 == sq2) pins.set(sq1);
    }
}

void Position::set_checkers_slow()
{
    assert(checkers_count_ == 0);

    const int king = king_sq();
    const int oside = flip_side(side_);

    const u8 opawn = make_pawn(oside);

    const int inc = pawn_inc(oside);

    if (const int orig = king - inc - 1; square(orig) == opawn)
        checkers_sq_[checkers_count_++] = orig;
    if (const int orig = king - inc + 1; square(orig) == opawn)
        checkers_sq_[checkers_count_++] = orig;

    for (int p12 = WN12 + oside; p12 <= BQ12; p12 += 2) {
        for (const int orig : piece_list_[p12]) {
            if (piece_attacks(orig, king)) {
                assert(checkers_count_ < 2);

                checkers_sq_[checkers_count_++] = orig;
            }
        }
    }
                
    assert(checkers_count_ <= 2);
}

void Position::set_checkers_fast(const Move& move)
{
    assert(checkers_count_ == 0);

    const int king = king_sq();
    const int orig = move.orig();
    const int dest = move.dest();

    const int oside = flip_side(side_);

    const u8 oflag = make_flag(oside);
    
    int sq;
    
    if (move.is_castle()) {
        const int rook = dest > orig ? orig + 1 : orig - 1;

        if (pseudo_attack(king, rook, RookFlag256)) {
            const int inc = delta_inc(king, rook);

            sq = king + inc;

            while (square(sq) == PieceNone256) sq += inc;

            if (sq == rook)
                checkers_sq_[checkers_count_++] = rook;
        }

        return;
    }
    
    if (move.is_ep()) {
        set_checkers_slow();

        return;
    }
    
    const int oinc = delta_inc(king, orig);
    const int dinc = delta_inc(king, dest);

    u8 piece256;

    // revealed check?

    if (oinc != dinc) {
        if (delta_type(oinc) & QueenFlags256) {
            sq = king + oinc;

            while ((piece256 = square(sq)) == PieceNone256) sq += oinc;

            if ((piece256 & oflag) && pseudo_attack(sq, king, piece256))
                checkers_sq_[checkers_count_++] = sq;
        }
    }

    // direct check?

    piece256 = square(dest);

    if (pseudo_attack(dest, king, piece256)) {
        if (is_slider(piece256)) {
            sq = king + dinc;
            
            while (square(sq) == PieceNone256) sq += dinc;

            if (sq != dest)
                return;
        }
        
        checkers_sq_[checkers_count_++] = dest;
    }
}

bool Position::empty(int orig, int dest) const
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

bool Position::move_is_legal(const Move& move) const
{
    if (Debug && (GenerateLegal || checkers_count_ > 0)) {
        Position pos(*this);

        Undo undo;

        pos.make_move(move, undo);

        assert(!pos.side_attacks(pos.side(), pos.king_sq(flip_side(pos.side()))));
    }

    if (GenerateLegal || checkers_count_ > 0)
        return true;

    const int king = king_sq();
    const int orig = move.orig();
    const int dest = move.dest();

    if (orig == king)
        return !side_attacks(flip_side(side_), dest);

    if (move.is_ep())
        return move_is_legal_ep(move);

    if (!pseudo_attack(orig, king, QueenFlags256))
        return true;

    if (!empty(orig, king))
        return true;

    const int pinc = delta_inc(king, orig);
    const int minc = delta_inc(orig, dest);

    if (abs(minc) == abs(pinc))
        return true;
    
    const int oside = flip_side(side_);

    const u8 oflag = make_flag(oside);

    int sq = orig;
    
    u8 piece256;

    do { sq += pinc; } while ((piece256 = square(sq)) == PieceNone256);

    if ((piece256 & oflag) == 0 || !pseudo_attack(king, sq, piece256))
        return true;

    return false;
}

bool Position::move_is_legal_ep(const Move& move) const
{
    assert(move.is_ep());
    assert(checkers_count_ == 0);

    const int king = king_sq();

    assert(move.orig() != king);

    const int orig = move.orig();
    const int dest = move.dest();

    const int mside = side_;
    
    const int inc = pawn_inc(mside);
    const int cap = dest - inc;

    const u8 oflag = make_flag(flip_side(mside));

    const int krank = sq88_rank(king);
    const int orank = sq88_rank(orig);

    const int oinc = delta_inc(king, orig);
    const int minc = orig - dest;
    const int cinc = delta_inc(king, cap);

    assert(abs(minc) == 15 || abs(minc) == 17);
    
    u8 piece256;

    //  king on same rank of capturing and captured pawn?

    if (krank == orank) {
        assert(oinc == cinc);

        int sq = king;

        // skip both pawns on rank

        do { sq += oinc; } while (sq == orig || sq == cap || (piece256 = square(sq)) == PieceNone256);

        // revealed checks not possible on any file or diagonal

        return (piece256 & oflag) == 0 || !pseudo_attack(king, sq, piece256);
    }

    // king on same diagonal of capturing pawn?

    if (pseudo_attack(king, orig, BishopFlag256)) {

        // same diagonal of capture line?

        if (oinc == minc || oinc == -minc)
            return true;

        if (!empty(king, orig))
            return true;

        int sq = orig;

        do { sq += oinc; } while ((piece256 = square(sq)) == PieceNone256);

        if ((piece256 & oflag) && (piece256 & BishopFlag256))
            return false;
    }
    
    // king on same diagonal of captured pawn?

    if (pseudo_attack(king, cap, BishopFlag256)) {
        if (!empty(king, cap))
            return true;

        int sq = cap;

        do { sq += cinc; } while ((piece256 = square(sq)) == PieceNone256);

        if ((piece256 & oflag) && (piece256 & BishopFlag256))
            return false;
    }

    const int kfile = sq88_file(king);
    const int ofile = sq88_file(orig);

    //  king on same file of capturing pawn?
    
    if (kfile == ofile) {
        assert(abs(oinc) == 16);

        if (!empty(king, orig))
            return true;

        int sq = orig;

        do { sq += oinc; } while ((piece256 = square(sq)) == PieceNone256);

        if ((piece256 & oflag) && (piece256 & RookFlag256))
            return false;
    }

    return true;
}
