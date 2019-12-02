#include <iostream>
#include "gen.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
#include "square.h"
#include "types.h"
using namespace std;

const constexpr int DeltaCount      = 240;
const constexpr int DeltaOffset     = 120;

static int DeltaIncrLUT[DeltaCount];
static int DeltaTypeLUT[DeltaCount];

static int CastleLUT[128];

static void gen_pawn_moves      (MoveList& moves, const Position& pos, const int orig);
static void gen_knight_moves    (MoveList& moves, const Position& pos, const int orig);
static void gen_bishop_moves    (MoveList& moves, const Position& pos, const int orig);
static void gen_rook_moves      (MoveList& moves, const Position& pos, const int orig);
static void gen_queen_moves     (MoveList& moves, const Position& pos, const int orig);

static void gen_king_moves      (MoveList& moves, const Position& pos, const bool castle);

static void gen_piece_moves     (MoveList& moves, const Position& pos, const int dest, const BitSet& pins);
static void gen_pawn_moves      (MoveList& moves, const Position& pos, const int dest, const BitSet& pins);

static void gen_pinned_moves    (MoveList& moves, const Position& pos, BitSet& pins);
static void gen_pinned_moves    (MoveList& moves, const Position& pos, int attacker, int pinned, int incr);

void gen_init()
{
    DeltaTypeLUT[DeltaOffset + 16 - 1] |= WhitePawnFlag256;
    DeltaTypeLUT[DeltaOffset + 16 + 1] |= WhitePawnFlag256;
    DeltaTypeLUT[DeltaOffset - 16 - 1] |= BlackPawnFlag256;
    DeltaTypeLUT[DeltaOffset - 16 + 1] |= BlackPawnFlag256;

    for (auto incr : KnightIncrs) {
        DeltaIncrLUT[DeltaOffset + incr] = incr;
        DeltaTypeLUT[DeltaOffset + incr] |= KnightFlag256;
    }

    for (auto incr : BishopIncrs) {
        for (int i = 1; i <= 7; i++) {
            DeltaIncrLUT[DeltaOffset + incr * i] = incr;
            DeltaTypeLUT[DeltaOffset + incr * i] |= BishopFlag256;
        }
    }

    for (auto incr : RookIncrs) {
        for (int i = 1; i <= 7; i++) {
            DeltaIncrLUT[DeltaOffset + incr * i] = incr;
            DeltaTypeLUT[DeltaOffset + incr * i] |= RookFlag256;
        }
    }

    for (auto incr : QueenIncrs)
        DeltaTypeLUT[DeltaOffset + incr] |= KingFlag256;

    for (int i = 0; i < 128; i++)
        CastleLUT[i] = ~0;

    CastleLUT[A1] = ~Position::WhiteCastleQFlag;
    CastleLUT[E1] = ~(Position::WhiteCastleQFlag | Position::WhiteCastleKFlag);
    CastleLUT[H1] = ~Position::WhiteCastleKFlag;

    CastleLUT[A8] = ~Position::BlackCastleQFlag;
    CastleLUT[E8] = ~(Position::BlackCastleQFlag | Position::BlackCastleKFlag);
    CastleLUT[H8] = ~Position::BlackCastleKFlag;
}

size_t gen_pseudo_moves(MoveList& moves, const Position& pos)
{
    assert(pos.checkers() == 0);

    const int side = pos.side();
    
    gen_king_moves(moves, pos, true);

    BitSet pins;

    if (GenerateLegal)
        gen_pinned_moves(moves, pos, pins);

    for (auto orig : pos.piece_list(WP12 + side)) {
        if (GenerateLegal && pins.test(orig)) continue;
        gen_pawn_moves(moves, pos, orig);
    }

    for (auto orig : pos.piece_list(WN12 + side)) {
        if (GenerateLegal && pins.test(orig)) continue;
        gen_knight_moves(moves, pos, orig);
    }

    for (auto orig : pos.piece_list(WB12 + side)) {
        if (GenerateLegal && pins.test(orig)) continue;
        gen_bishop_moves(moves, pos, orig);
    }

    for (auto orig : pos.piece_list(WR12 + side)) {
        if (GenerateLegal && pins.test(orig)) continue;
        gen_rook_moves(moves, pos, orig);
    }

    for (auto orig : pos.piece_list(WQ12 + side)) {
        if (GenerateLegal && pins.test(orig)) continue;
        gen_queen_moves(moves, pos, orig);
    }

    return moves.size();
}

size_t gen_legal_moves(MoveList& moves, const Position& pos)
{
    assert(pos.is_ok() == 0);
    assert(moves.empty());

    const size_t count = pos.checkers() > 0 
                       ? gen_evasion_moves(moves, pos)
                       : gen_pseudo_moves(moves, pos);

    if (Debug && (GenerateLegal || pos.checkers() > 0)) {
        Position p(pos);

        Undo undo;

        for (const auto& m : moves) {
            p.make_move(m, undo);
            assert(!p.side_attacks(p.side(), p.king_sq(flip_side(p.side()))));
            p.unmake_move(m, undo);
        }
    }

    return count;
}

void gen_pawn_moves(MoveList& moves, const Position& pos, const int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Pawn));

    const u8 oflag = make_flag(flip_side(pos.side()));

    const int incr = pawn_incr(pos.side());
    const int rank = sq88_rank(orig, pos.side());

    assert(rank >= Rank2 && rank <= Rank7);

    // promotions

    if (rank == Rank7) {
        if (int dest = orig + incr - 1; pos[dest] & oflag) {
            Move m(orig, dest, pos[dest]);

            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }

        if (int dest = orig + incr + 1; pos[dest] & oflag) {
            Move m(orig, dest, pos[dest]);

            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }

        if (int dest = orig + incr; pos.empty(dest)) {
            Move m(orig, dest);

            moves.add(m | Move::PromoKnightFlag);
            moves.add(m | Move::PromoBishopFlag);
            moves.add(m | Move::PromoRookFlag);
            moves.add(m | Move::PromoQueenFlag);
        }
    }
    else {
        if (int ep_sq = pos.ep_sq(); ep_sq != SquareNone) {
            if (orig == ep_dual(ep_sq) - 1 || orig == ep_dual(ep_sq) + 1) {
                Move m = Move(orig, ep_sq, pos[ep_dual(ep_sq)]) | Move::EPFlag;

                if (!GenerateLegal || pos.move_is_legal_ep(m))
                    moves.add(m);
            }
        }

        if (int dest = orig + incr - 1; pos[dest] & oflag)
            moves.add(Move(orig, dest, pos[dest]));

        if (int dest = orig + incr + 1; pos[dest] & oflag)
            moves.add(Move(orig, dest, pos[dest]));

        // single push
        
        if (int dest = orig + incr; pos.empty(dest)) {
            moves.add(Move(orig, dest));

            // double push

            dest += incr;

            if (rank == Rank2 && pos.empty(dest))
                moves.add(Move(orig, dest) | Move::DoubleFlag);
        }
    }
}

void gen_knight_moves(MoveList& moves, const Position& pos, const int orig)
{
    assert(sq88_is_ok(orig));
    assert(pos.is_me(orig));
    assert(pos.is_piece(orig, Knight));

    const u8 mflag = make_flag(pos.side());

    for (auto incr : KnightIncrs) {
        const int dest = orig + incr;
        
        if (!sq88_is_ok(dest)) continue;

        const u8 piece = pos[dest];

        if ((piece & mflag) == 0)
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

    for (auto incr : BishopIncrs) {
        int dest = orig + incr;

        for ( ; (piece = pos[dest]) == PieceNone256; dest += incr)
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

    for (auto incr : RookIncrs) {
        int dest = orig + incr;

        for ( ; (piece = pos[dest]) == PieceNone256; dest += incr)
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

    for (auto incr : QueenIncrs) {
        int dest = orig + incr;

        for ( ; (piece = pos[dest]) == PieceNone256; dest += incr)
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

    const int mside =           pos.side();
    const int oside = flip_side(pos.side());
    
    const u8 mflag = make_flag(mside);

    for (auto incr : QueenIncrs) {
        const int dest = king + incr;
        
        if (!sq88_is_ok(dest)) continue;

        const u8 piece = pos[dest];

        if ((piece & mflag) == 0)
            if (!GenerateLegal || !pos.side_attacks(oside, dest))
                moves.add(Move(king, dest, piece));
    }

    if (castle) {
        if (pos.can_castle_k()) {
            int king1 = king + 1;
            int king2 = king + 2;

            if (pos.empty(king1) && pos.empty(king2)) {
                bool legal = !pos.side_attacks(oside, king1)
                          && (!GenerateLegal || !pos.side_attacks(oside, king2));

                if (legal)
                    moves.add(Move(king, king2) | Move::CastleFlag);
            }
        }
        if (pos.can_castle_q()) {
            int king1 = king - 1;
            int king2 = king - 2;
            int king3 = king - 3;

            if (pos.empty(king1) && pos.empty(king2) && pos.empty(king3)) {
                bool legal = !pos.side_attacks(oside, king1)
                          && (!GenerateLegal || !pos.side_attacks(oside, king2));

                if (legal)
                    moves.add(Move(king, king2) | Move::CastleFlag);
            }
        }
    }
}

size_t gen_evasion_moves(MoveList& moves, const Position& pos)
{
    const int king = pos.king_sq();

    assert(pos.checkers() > 0);

    const int checker1 =                      pos.checkers(0);
    const int checker2 = pos.checkers() > 1 ? pos.checkers(1) : SquareNone;

    const int incr1 =                           is_slider(pos[checker1]) ? delta_incr(king, checker1) : 0;
    const int incr2 = checker2 != SquareNone && is_slider(pos[checker2]) ? delta_incr(king, checker2) : 0;
    
    const int mside = pos.side();
    const int oside = flip_side(pos.side());
    
    const u8 mflag = make_flag(mside);

    for (auto incr : QueenIncrs) {
        if (incr == -incr1 || incr == -incr2)
            continue;

        const int dest = king + incr;

        if (!sq88_is_ok(dest)) continue;

        const u8 piece = pos[dest];

        if ((piece & mflag) == 0)
            if (!pos.side_attacks(oside, dest))
                moves.add(Move(king, dest, piece));
    }

    if (pos.checkers() == 2)
        return moves.size();

    BitSet pins;

    pos.mark_pins(pins);

    const u8 mpawn = make_pawn(mside);
    const u8 opawn = make_pawn(oside);

    const int rank = sq88_rank(checker1, mside);
    const int incr = pawn_incr(mside);

    // pawn captures checking piece

    if (checker1 == ep_dual(pos.ep_sq())) {
        if (int orig = checker1 - 1; pos[orig] == mpawn && !pins.test(orig))
            moves.add(Move(orig, pos.ep_sq(), opawn) | Move::EPFlag);
        
        if (int orig = checker1 + 1; pos[orig] == mpawn && !pins.test(orig))
            moves.add(Move(orig, pos.ep_sq(), opawn) | Move::EPFlag);
    }

    if (int orig = checker1 - incr - 1; pos[orig] == mpawn && !pins.test(orig)) {
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

    if (int orig = checker1 - incr + 1; pos[orig] == mpawn && !pins.test(orig)) {
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

    for (int p12 = WN12 + mside; p12 <= BQ12; p12 += 2) {
        for (auto orig : pos.piece_list(p12)) {
            if (pins.test(orig))
                continue;

            const u8 piece = pos[orig];

            if (!pseudo_attack(orig, checker1, piece))
                continue;

            if (is_slider(piece) && !pos.empty(orig, checker1))
                continue;

            moves.add(Move(orig, checker1, pos[checker1]));
        }
    }

    // blockers if slider

    if (is_slider(pos[checker1])) {
        for (int sq = king + incr1; sq != checker1; sq += incr1) {
            gen_pawn_moves(moves, pos, sq, pins);
            gen_piece_moves(moves, pos, sq, pins);
        }
    }

    return moves.size();
}

void gen_piece_moves(MoveList& moves, const Position& pos, const int dest, const BitSet& pins)
{
    const int mside = pos.side();

    for (int orig : pos.piece_list(WN12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (pseudo_attack(orig, dest, KnightFlag256))
            moves.add(Move(orig, dest, pos[dest]));
    }

    for (int orig : pos.piece_list(WB12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (!pseudo_attack(orig, dest, BishopFlag256))
            continue;

        if (!pos.empty(orig, dest))
            continue;

        moves.add(Move(orig, dest, pos[dest]));
    }

    for (int orig : pos.piece_list(WR12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (!pseudo_attack(orig, dest, RookFlag256))
            continue;

        if (!pos.empty(orig, dest))
            continue;

        moves.add(Move(orig, dest, pos[dest]));
    }

    for (int orig : pos.piece_list(WQ12 + mside)) {
        assert(sq88_is_ok(orig));

        if (pins.test(orig))
            continue;

        if (!pseudo_attack(orig, dest, QueenFlags256))
            continue;

        if (!pos.empty(orig, dest))
            continue;

        moves.add(Move(orig, dest, pos[dest]));
    }
}

void gen_pawn_moves(MoveList& moves, const Position& pos, const int dest, const BitSet& pins)
{
    const int mside = pos.side();
    const int incr = pawn_incr(mside);
    const int rank = sq88_rank(dest, mside);

    const u8 mpawn = make_pawn(mside);
        
    int orig;

    // double push

    if (rank == Rank4) {
        orig = dest - incr - incr;

        if (pos[orig] == mpawn && !pins.test(orig))
            if (pos[orig + incr] == PieceNone256)
                moves.add(Move(orig, dest) | Move::DoubleFlag);
    }

    // single push

    orig = dest - incr;

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

void gen_pinned_moves(MoveList& moves, const Position& pos, int attacker, int pinned, int incr)
{
    const int king = pos.king_sq();

    assert(sq88_is_ok(attacker));
    assert(sq88_is_ok(pinned));
    assert(sq88_is_ok(king));
    assert(incr != 0);
    assert(pos.is_op(attacker));
    assert(pos.is_me(pinned));
    assert(pos.king_sq() == king);
    assert(is_slider(pos[attacker]));

    const u8 mpiece256 = pos[pinned];
    const u8 opiece256 = pos[attacker];

    int sq;

    if (is_pawn(mpiece256)) {
        const int mside = pos.side();
        const int pincr = pawn_incr(mside);
        const int rank = sq88_rank(pinned, mside);
        const int ep_sq = pos.ep_sq();
        
        sq = pinned + pincr;

        if (sq - 1 == ep_sq || sq + 1 == ep_sq) {
            if (delta_incr(pinned, ep_sq) == -incr) {
                Move m = Move(pinned, ep_sq, pos[ep_dual(ep_sq)]) | Move::EPFlag;

                if (!GenerateLegal || pos.move_is_legal_ep(m))
                    moves.add(m);
            }
        }

        // vertical pinned
        
        if (abs(incr) == 16) {
            if (pos.empty(sq)) {
                moves.add(Move(pinned, sq));

                sq += pincr;

                if (rank == Rank2 && pos.empty(sq))
                    moves.add(Move(pinned, sq) | Move::DoubleFlag);
            }
        }
        else if (sq - 1 == attacker || sq + 1 == attacker) {
            Move m(pinned, attacker, opiece256);
                
            if (rank == Rank7) {
                moves.add(m | Move::PromoKnightFlag);
                moves.add(m | Move::PromoBishopFlag);
                moves.add(m | Move::PromoRookFlag);
                moves.add(m | Move::PromoQueenFlag);
            }
            else
                moves.add(m);
        }
    }
    else if (pseudo_attack(pinned, attacker, mpiece256)) {
        assert(is_slider(mpiece256));

        for (sq = king   - incr; pos.empty(sq); sq -= incr) moves.add(Move(pinned, sq));
        for (sq = pinned - incr; pos.empty(sq); sq -= incr) moves.add(Move(pinned, sq));

        moves.add(Move(pinned, attacker, opiece256));
    }
}

void gen_pinned_moves(MoveList& moves, const Position& pos, BitSet& pins)
{
    assert(pins.none());

    const int king = pos.king_sq();
    const int oside = flip_side(pos.side());

    const u8 mflag = make_flag(pos.side());

    int incr;
    int pinned;
    int sq;
    
    for (const auto attacker : pos.piece_list(WB12 + oside)) {
        if (!pseudo_attack(attacker, king, BishopFlag256)) continue;
        incr = delta_incr(attacker, king);
        sq = attacker + incr;
        while (pos[sq] == PieceNone256) sq += incr;
        if ((pos[sq] & mflag) == 0) continue;
        pinned = sq;
        sq = king - incr;
        while (pos[sq] == PieceNone256) sq -= incr;
        if (sq != pinned) continue;
        gen_pinned_moves(moves, pos, attacker, pinned, incr);
        pins.set(pinned);
    }
    
    for (const auto attacker : pos.piece_list(WR12 + oside)) {
        if (!pseudo_attack(attacker, king, RookFlag256)) continue;
        incr = delta_incr(attacker, king);
        sq = attacker + incr;
        while (pos[sq] == PieceNone256) sq += incr;
        if ((pos[sq] & mflag) == 0) continue;
        pinned = sq;
        sq = king - incr;
        while (pos[sq] == PieceNone256) sq -= incr;
        if (sq != pinned) continue;
        gen_pinned_moves(moves, pos, attacker, pinned, incr);
        pins.set(pinned);
    }
    
    for (const auto attacker : pos.piece_list(WQ12 + oside)) {
        if (!pseudo_attack(attacker, king, QueenFlags256)) continue;
        incr = delta_incr(attacker, king);
        sq = attacker + incr;
        while (pos[sq] == PieceNone256) sq += incr;
        if ((pos[sq] & mflag) == 0) continue;
        pinned = sq;
        sq = king - incr;
        while (pos[sq] == PieceNone256) sq -= incr;
        if (sq != pinned) continue;
        gen_pinned_moves(moves, pos, attacker, pinned, incr);
        pins.set(pinned);
    }
}

int delta_incr(int orig, int dest)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return DeltaIncrLUT[DeltaOffset + dest - orig];
}

u8 pseudo_attack(int incr)
{
    assert(incr >= -DeltaOffset && incr < DeltaOffset);

    return DeltaTypeLUT[DeltaOffset + incr];
}

bool pseudo_attack(int orig, int dest, u8 piece)
{
    assert(sq88_is_ok(orig));
    assert(sq88_is_ok(dest));

    return DeltaTypeLUT[DeltaOffset + dest - orig] & piece;
}

int castle_flag(int sq)
{
    assert(sq88_is_ok(sq));

    return CastleLUT[sq];
}
