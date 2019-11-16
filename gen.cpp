#include <iostream>
#include "gen.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
#include "types.h"
using namespace std;

static int delta_inc_lut[256];
static int delta_type_lut[256];
static int castle_lut[256];

static void gen_pawn_moves      (MoveList& moves, const Position& pos, int orig);
static void gen_knight_moves    (MoveList& moves, const Position& pos, int orig);
static void gen_bishop_moves    (MoveList& moves, const Position& pos, int orig);
static void gen_rook_moves      (MoveList& moves, const Position& pos, int orig);
static void gen_queen_moves     (MoveList& moves, const Position& pos, int orig);

static void gen_king_castles    (MoveList& moves, const Position& pos);
static void gen_king_moves      (MoveList& moves, const Position& pos);

static void gen_nbrq_moves      (MoveList& moves, const Position& pos, int dest, const BitSet& pins);

void gen_init()
{
    delta_type_lut[128 + 16 - 1] |= WhitePawnFlag256;
    delta_type_lut[128 + 16 + 1] |= WhitePawnFlag256;
    delta_type_lut[128 - 16 - 1] |= BlackPawnFlag256;
    delta_type_lut[128 - 16 + 1] |= BlackPawnFlag256;

    for (auto inc : KnightIncs) {
        delta_inc_lut[128 + inc] = inc;
        delta_type_lut[128 + inc] |= KnightFlag256;
    }

    for (auto inc : BishopIncs) {
        for (int i = 1; i <= 7; i++) {
            delta_inc_lut[128 + inc * i] = inc;
            delta_type_lut[128 + inc * i] |= BishopFlag256;
        }
    }

    for (auto inc : RookIncs) {
        for (int i = 1; i <= 7; i++) {
            delta_inc_lut[128 + inc * i] = inc;
            delta_type_lut[128 + inc * i] |= RookFlag256;
        }
    }

    for (auto inc : QueenIncs)
        delta_type_lut[128 + inc] |= KingFlag256;

    for (int i = 0; i < 256; i++)
        castle_lut[i] = ~0;

    castle_lut[A1] = ~Position::WhiteCastleQFlag;
    castle_lut[E1] = ~(Position::WhiteCastleQFlag | Position::WhiteCastleKFlag);
    castle_lut[H1] = ~Position::WhiteCastleKFlag;

    castle_lut[A8] = ~Position::BlackCastleQFlag;
    castle_lut[E8] = ~(Position::BlackCastleQFlag | Position::BlackCastleKFlag);
    castle_lut[H8] = ~Position::BlackCastleKFlag;
}

size_t gen_pseudo_moves(MoveList& moves, const Position& pos)
{
    //assert(pos.is_ok() == 0);

    const int side = pos.side();

    gen_king_moves(moves, pos);
    
    if (pos.checkers() == 2)
        return moves.size();

    if (pos.checkers() == 0)
        gen_king_castles(moves, pos);

    for (auto orig : pos.piece_list(PieceList12[side][Pawn]))
        gen_pawn_moves(moves, pos, orig);

    for (auto orig : pos.piece_list(PieceList12[side][Knight]))
        gen_knight_moves(moves, pos, orig);

    for (auto orig : pos.piece_list(PieceList12[side][Bishop]))
        gen_bishop_moves(moves, pos, orig);

    for (auto orig : pos.piece_list(PieceList12[side][Rook]))
        gen_rook_moves(moves, pos, orig);

    for (auto orig : pos.piece_list(PieceList12[side][Queen]))
        gen_queen_moves(moves, pos, orig);

    return moves.size();
}

//size_t gen_legal_moves(MoveList& moves, const Position& pos)
size_t gen_legal_moves(MoveList& moves, const Position&)
{
    return moves.size();
}

void gen_pawn_moves(MoveList& moves, const Position& pos, int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Pawn));

    u8 oflag = make_flag(flip_side(pos.side()));

    const int inc = pawn_inc(pos.side());
    const int rank = sq88_rank(orig, pos.side());

    assert(rank >= Rank2 && rank <= Rank7);

    // promotions

    if (rank == Rank7) {
        if (int dest = orig + inc - 1; pos[dest] & oflag) {
            Move m(orig, dest, pos[dest]);

            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }

        if (int dest = orig + inc + 1; pos[dest] & oflag) {
            Move m(orig, dest, pos[dest]);

            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }

        if (int dest = orig + inc; pos.is_empty(dest)) {
            Move m(orig, dest);

            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }
    }
    else {
        if (int ep_sq = pos.ep_sq(); rank == Rank5 && ep_sq != SquareNone) {
            if (abs(sq88_file(orig) - sq88_file(ep_sq)) == 1)
                moves.add(Move(orig, ep_sq, pos[ep_sq - inc]) | Move::EPFlag);
        }

        if (int dest = orig + inc - 1; pos[dest] & oflag)
            moves.add(Move(orig, dest, pos[dest]));

        if (int dest = orig + inc + 1; pos[dest] & oflag)
            moves.add(Move(orig, dest, pos[dest]));

        // single push
        
        if (int dest = orig + inc; pos.is_empty(dest)) {
            moves.add(Move(orig, dest));

            // double push

            dest += inc;

            if (rank == Rank2 && pos.is_empty(dest))
                moves.add(Move(orig, dest) | Move::DoubleFlag);
        }
    }
}

void gen_knight_moves(MoveList& moves, const Position& pos, int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Knight));

    u8 oflag = make_flag(flip_side(pos.side()));
    u8 piece;

    for (auto inc : KnightIncs) {
        int dest = orig;

        piece = pos[dest += inc];

        if (piece == PieceNone256)
            moves.add(Move(orig, dest));
        else if (piece & oflag)
            moves.add(Move(orig, dest, piece));
    }
}

void gen_bishop_moves(MoveList& moves, const Position& pos, int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Bishop));

    u8 oflag = make_flag(flip_side(pos.side()));
    u8 piece;

    for (auto inc : BishopIncs) {
        int dest = orig;

        while ((piece = pos[dest += inc]) == PieceNone256)
            moves.add(Move(orig, dest));

        if (piece & oflag)
            moves.add(Move(orig, dest, piece));
    }
}

void gen_rook_moves(MoveList& moves, const Position& pos, int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Rook));
    
    u8 oflag = make_flag(flip_side(pos.side()));
    u8 piece;

    for (auto inc : RookIncs) {
        int dest = orig;

        while ((piece = pos[dest += inc]) == PieceNone256)
            moves.add(Move(orig, dest));

        if (piece & oflag)
            moves.add(Move(orig, dest, piece));
    }
}

void gen_queen_moves(MoveList& moves, const Position& pos, int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Queen));

    u8 oflag = make_flag(flip_side(pos.side()));
    u8 piece;

    for (auto inc : QueenIncs) {
        int dest = orig;

        while ((piece = pos[dest += inc]) == PieceNone256)
            moves.add(Move(orig, dest));

        if (piece & oflag)
            moves.add(Move(orig, dest, piece));
    }
}

void gen_king_moves(MoveList& moves, const Position& pos)
{
    int king = pos.king_sq();

    assert(sq88_is_ok(king));
    assert(pos.is_me(king));
    assert(pos.is_piece(king, King));

    const int mside = pos.side();
    const int oside = flip_side(pos.side());
    
    u8 mflag = make_flag(mside);
    u8 piece;

    for (auto inc : QueenIncs) {
        int dest = king + inc;

        if (!sq88_is_ok(dest))
            continue;

        piece = pos[dest];

        if (piece & mflag)
            continue;

        // FIXME: doesn't prevent me from walking in line of checking slider
        //if (pos.side_attacks(oside, dest))
            //continue;

        moves.add(Move(king, dest, piece));
    }
}

void gen_king_castles(MoveList& moves, const Position& pos)
{
    int king = pos.king_sq();

    assert(sq88_is_ok(king));
    assert(pos.is_me(king));
    assert(pos.is_piece(king, King));

    const int oside = flip_side(pos.side());
    
    if (pos.can_castle_k()) {
        if (   pos.is_empty(king + 1)
            && pos.is_empty(king + 2)) {

            bool check = pos.side_attacks(oside, king + 1)
                      || pos.side_attacks(oside, king + 2);

            if (!check)
                moves.add(Move(king, king + 2) | Move::CastleFlag);
        }
    }
    if (pos.can_castle_q()) {
        if (   pos.is_empty(king - 1)
            && pos.is_empty(king - 2)
            && pos.is_empty(king - 3)) {

            bool check = pos.side_attacks(oside, king - 1)
                      || pos.side_attacks(oside, king - 2);

            if (!check)
                moves.add(Move(king, king - 2) | Move::CastleFlag);
        }
    }
}

void gen_nbrq_moves(MoveList& moves, const Position& pos, int dest, const bitset<128>& pins)
{
    const int mside = pos.side();

    for (const int orig : pos.piece_list(WhiteKnight12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (delta_type(orig, dest) & KnightFlag256)
            moves.add(Move(orig, dest, pos[dest]));
    }

    for (const int orig : pos.piece_list(WhiteBishop12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if ((delta_type(orig, dest) & BishopFlag256) == 0)
            continue;

        if (int inc = delta_inc(dest, orig); inc != 0) {
            int sq = dest;

            do { sq += inc; } while (pos[sq] == PieceNone256);

            if (sq == orig)
                moves.add(Move(orig, dest, pos[dest]));
        }
    }

    for (const int orig : pos.piece_list(WhiteRook12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if ((delta_type(orig, dest) & RookFlag256) == 0)
            continue;

        if (int inc = delta_inc(dest, orig); inc != 0) {
            int sq = dest;

            do { sq += inc; } while (pos[sq] == PieceNone256);

            if (sq == orig)
                moves.add(Move(orig, dest, pos[dest]));
        }
    }

    for (const int orig : pos.piece_list(WhiteQueen12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if ((delta_type(orig, dest) & QueenFlags256) == 0)
            continue;

        if (int inc = delta_inc(dest, orig); inc != 0) {
            int sq = dest;

            do { sq += inc; } while (pos[sq] == PieceNone256);

            if (sq == orig)
                moves.add(Move(orig, dest, pos[dest]));
        }
    }
}

int delta_inc(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_inc_lut[128 + dest - orig];
}

u8 delta_type(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_type_lut[128 + dest - orig];
}

bool pseudo_attack(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_type_lut[128 + dest - orig];
}

bool pseudo_attack(int orig, int dest, u8 piece)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_type_lut[128 + dest - orig] & piece;
}

int castle_flag(int sq)
{
    assert(sq88_is_ok(sq));

    return castle_lut[sq];
}
