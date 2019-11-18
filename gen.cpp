#include <iostream>
#include "gen.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
#include "types.h"
using namespace std;

const constexpr int DeltaCount = 240;
const constexpr int DeltaOffset = 120;

static int delta_inc_lut[DeltaCount];
static int delta_type_lut[DeltaCount];

static int castle_lut[128];

static void gen_pawn_moves      (MoveList& moves, const Position& pos, const int orig);
static void gen_knight_moves    (MoveList& moves, const Position& pos, const int orig);
static void gen_bishop_moves    (MoveList& moves, const Position& pos, const int orig);
static void gen_rook_moves      (MoveList& moves, const Position& pos, const int orig);
static void gen_queen_moves     (MoveList& moves, const Position& pos, const int orig);

static void gen_king_moves      (MoveList& moves, const Position& pos, const bool castle);

static void gen_piece_moves     (MoveList& moves, const Position& pos, const int dest, const BitSet& pins);
static void gen_pawn_moves      (MoveList& moves, const Position& pos, const int dest, const BitSet& pins);

void gen_init()
{
    delta_type_lut[DeltaOffset + 16 - 1] |= WhitePawnFlag256;
    delta_type_lut[DeltaOffset + 16 + 1] |= WhitePawnFlag256;
    delta_type_lut[DeltaOffset - 16 - 1] |= BlackPawnFlag256;
    delta_type_lut[DeltaOffset - 16 + 1] |= BlackPawnFlag256;

    for (auto inc : KnightIncs) {
        delta_inc_lut[DeltaOffset + inc] = inc;
        delta_type_lut[DeltaOffset + inc] |= KnightFlag256;
    }

    for (auto inc : BishopIncs) {
        for (int i = 1; i <= 7; i++) {
            delta_inc_lut[DeltaOffset + inc * i] = inc;
            delta_type_lut[DeltaOffset + inc * i] |= BishopFlag256;
        }
    }

    for (auto inc : RookIncs) {
        for (int i = 1; i <= 7; i++) {
            delta_inc_lut[DeltaOffset + inc * i] = inc;
            delta_type_lut[DeltaOffset + inc * i] |= RookFlag256;
        }
    }

    for (auto inc : QueenIncs)
        delta_type_lut[DeltaOffset + inc] |= KingFlag256;

    for (int i = 0; i < 128; i++)
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
    assert(pos.checkers() == 0);

    const int mside = pos.side();
    const int oside = flip_side(pos.side());
    
    const int king = pos.king_sq();

    const u8 mflag = make_flag(mside);
    
    u8 piece256;

    BitSet pins;

    int sq1;
    int sq2;
    int inc;

    gen_king_moves(moves, pos, true);

    for (int p12 = WhiteBishop12 + oside; p12 <= BlackQueen12; p12 += 2) {
        for (const int orig : pos.piece_list(p12)) {
            assert(sq88_is_ok(orig));

            piece256 = pos[orig];

            if (!pseudo_attack(king, orig, piece256))
                continue;

            inc = delta_inc(king, orig);
            sq1 = king + inc;

            while ((piece256 = pos[sq1]) == PieceNone256) sq1 += inc;
            
            assert(sq88_is_ok(sq1));

            if ((piece256 & mflag) == 0)
                continue;

            // skip pawns only

            if (is_pawn(piece256))
                continue;

            sq2 = orig - inc;

            while ((piece256 = pos[sq2]) == PieceNone256) sq2 -= inc;

            assert(sq88_is_ok(sq2));

            if (sq1 != sq2)
                continue;
            
            if (pseudo_attack(sq1, orig, piece256)) {
                asdf



                continue;
            }
                
            pins.set(sq1);
        }
    }
    


    for (auto orig : pos.piece_list(PieceList12[mside][Pawn])) gen_pawn_moves(moves, pos, orig);
    for (auto orig : pos.piece_list(PieceList12[mside][Knight])) gen_knight_moves(moves, pos, orig);
    for (auto orig : pos.piece_list(PieceList12[mside][Bishop])) gen_bishop_moves(moves, pos, orig);
    for (auto orig : pos.piece_list(PieceList12[mside][Rook])) gen_rook_moves(moves, pos, orig);
    for (auto orig : pos.piece_list(PieceList12[mside][Queen])) gen_queen_moves(moves, pos, orig);

    return moves.size();
}

//size_t gen_legal_moves(MoveList& moves, const Position& pos)
size_t gen_legal_moves(MoveList& moves, const Position&)
{
    return moves.size();
}

void gen_pawn_moves(MoveList& moves, const Position& pos, const int orig)
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
        // TODO FIXME
        // FIXME TODO
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

void gen_knight_moves(MoveList& moves, const Position& pos, const int orig)
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

void gen_bishop_moves(MoveList& moves, const Position& pos, const int orig)
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

void gen_rook_moves(MoveList& moves, const Position& pos, const int orig)
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

void gen_queen_moves(MoveList& moves, const Position& pos, const int orig)
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

void gen_king_moves(MoveList& moves, const Position& pos, const bool castle)
{
    const int king = pos.king_sq();

    assert(pos.is_me(king));
    assert(pos.is_piece(king, King));

    const int oside = flip_side(pos.side());
    
    const u8 oflag = make_flag(oside);

    for (auto inc : QueenIncs) {
        const int dest = king + inc;

        const u8 piece = pos[dest];

        if (piece == PieceNone256 || (piece & oflag))
            if (!pos.side_attacks(oside, dest))
                moves.add(Move(king, dest, piece));
    }

    if (castle) {
        if (pos.can_castle_k()) {
            int king1 = king + 1;
            int king2 = king + 2;

            if (pos.is_empty(king1) && pos.is_empty(king2)) {
                bool legal = !pos.side_attacks(oside, king1) && !pos.side_attacks(oside, king2);

                if (legal)
                    moves.add(Move(king, king2) | Move::CastleFlag);
            }
        }
        if (pos.can_castle_q()) {
            int king1 = king - 1;
            int king2 = king - 2;
            int king3 = king - 3;

            if (pos.is_empty(king1) && pos.is_empty(king2) && pos.is_empty(king3)) {
                bool legal = !pos.side_attacks(oside, king1) && !pos.side_attacks(oside, king2);

                if (legal)
                    moves.add(Move(king, king2) | Move::CastleFlag);
            }
        }
    }
}

size_t gen_evasion_moves(MoveList& moves, const Position& pos)
{
    const int king = pos.king_sq();

    const int checkers_count = pos.checkers();

    assert(checkers_count > 0);

    const int checker1 =                      pos.checkers(0);
    const int checker2 = checkers_count > 1 ? pos.checkers(1) : SquareNone;

    const int inc1 =                           is_slider(pos[checker1]) ? delta_inc(king, checker1) : 0;
    const int inc2 = checker2 != SquareNone && is_slider(pos[checker2]) ? delta_inc(king, checker2) : 0;
    
    const int mside = pos.side();
    const int oside = flip_side(pos.side());
    
    const u8 oflag = make_flag(oside);

    for (auto inc : QueenIncs) {
        if (inc == -inc1 || inc == -inc2)
            continue;

        const int dest = king + inc;

        const u8 piece = pos[dest];

        if (piece == PieceNone256 || (piece & oflag))
            if (!pos.side_attacks(oside, dest))
                moves.add(Move(king, dest, piece));
    }

    if (checkers_count == 2)
        return moves.size();

    BitSet pins;

    pos.mark_pins(pins);

    const u8 mpawn = make_pawn(mside);
    const u8 opawn = make_pawn(oside);

    const int rank = sq88_rank(checker1, mside);
    const int inc = pawn_inc(mside);

    int pawn;

    // pawn captures checking piece

    if (checker1 == pos.ep_sq() - inc) {
        pawn = checker1 - 1;

        if (pos[pawn] == mpawn && !pins.test(pawn))
            moves.add(Move(pawn, pos.ep_sq(), opawn) | Move::EPFlag);
        
        pawn = checker1 + 1;
        
        if (pos[pawn] == mpawn && !pins.test(pawn))
            moves.add(Move(pawn, pos.ep_sq(), opawn) | Move::EPFlag);
    }

    if (int orig = checker1 - inc - 1; pos[orig] == mpawn && !pins.test(orig)) {
        Move m(orig, checker1, pos[checker1]);

        if (rank == Rank8) {
            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }
        else
            moves.add(m);
    }

    if (int orig = checker1 - inc + 1; pos[orig] == mpawn && !pins.test(orig)) {
        Move m(orig, checker1, pos[checker1]);

        if (rank == Rank8) {
            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }
        else
            moves.add(m);
    }


    // piece captures checking piece

    for (int p12 = WhiteKnight12 + mside; p12 <= BlackQueen12; p12 += 2) {
        for (auto orig : pos.piece_list(p12)) {
            if (pins.test(orig))
                continue;

            const u8 piece = pos[orig];

            if (!pseudo_attack(orig, checker1, piece))
                continue;

            if (is_slider(piece) && !pos.is_empty(orig, checker1))
                continue;

            moves.add(Move(orig, checker1, pos[checker1]));
        }
    }

    // blockers if slider

    if (is_slider(pos[checker1])) {
        for (int sq = king + inc1; sq != checker1; sq += inc1) {
            gen_pawn_moves(moves, pos, sq, pins);
            gen_piece_moves(moves, pos, sq, pins);
        }
    }

    return moves.size();
}

void gen_piece_moves(MoveList& moves, const Position& pos, const int dest, const BitSet& pins)
{
    const int mside = pos.side();

    for (int orig : pos.piece_list(WhiteKnight12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (pseudo_attack(orig, dest, KnightFlag256))
            moves.add(Move(orig, dest, pos[dest]));
    }

    for (int orig : pos.piece_list(WhiteBishop12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (!pseudo_attack(orig, dest, BishopFlag256))
            continue;

        if (!pos.is_empty(orig, dest))
            continue;

        moves.add(Move(orig, dest, pos[dest]));
    }

    for (int orig : pos.piece_list(WhiteRook12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (!pseudo_attack(orig, dest, RookFlag256))
            continue;

        if (!pos.is_empty(orig, dest))
            continue;

        moves.add(Move(orig, dest, pos[dest]));
    }

    for (int orig : pos.piece_list(WhiteQueen12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (!pseudo_attack(orig, dest, QueenFlags256))
            continue;

        if (!pos.is_empty(orig, dest))
            continue;

        moves.add(Move(orig, dest, pos[dest]));
    }
}

void gen_pawn_moves(MoveList& moves, const Position& pos, const int dest, const BitSet& pins)
{
    const int mside = pos.side();
    const int inc = pawn_inc(mside);
    const int rank = sq88_rank(dest, mside);

    const u8 mpawn = make_pawn(mside);
        
    int orig;

    // double push

    if (rank == Rank4) {
        orig = dest - inc - inc;

        if (pos[orig] == mpawn && !pins.test(orig))
            if (pos[orig + inc] == PieceNone256)
                moves.add(Move(orig, dest) | Move::DoubleFlag);
    }

    // single push

    orig = dest - inc;

    if (pos[orig] == mpawn && !pins.test(orig)) {
        Move m(orig, dest);

        if (rank == Rank8) {
            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }
        else
            moves.add(m);
    }
}

int delta_inc(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_inc_lut[DeltaOffset + dest - orig];
}

u8 delta_type(int inc)
{
    assert(inc >= -DeltaOffset && inc < DeltaOffset);

    return delta_type_lut[DeltaOffset + inc];
}

u8 delta_type(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_type_lut[DeltaOffset + dest - orig];
}

bool pseudo_attack(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_type_lut[DeltaOffset + dest - orig];
}

bool pseudo_attack(int orig, int dest, u8 piece)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return delta_type_lut[DeltaOffset + dest - orig] & piece;
}

int castle_flag(int sq)
{
    assert(sq88_is_ok(sq));

    return castle_lut[sq];
}
