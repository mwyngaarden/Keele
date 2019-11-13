#include <bitset>
#include <iostream>
#include "gen.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
using namespace std;

namespace Gen {

    static int inc88[256];
    static int type88[256];
    static int castle88[256];

    static void gen_pawn_moves     (Move::List& moves, const Position& pos, int orig);
    static void gen_knight_moves   (Move::List& moves, const Position& pos, int orig);
    static void gen_bishop_moves   (Move::List& moves, const Position& pos, int orig);
    static void gen_rook_moves     (Move::List& moves, const Position& pos, int orig);
    static void gen_queen_moves    (Move::List& moves, const Position& pos, int orig);

    static void gen_king_castles   (Move::List& moves, const Position& pos);
    static void gen_king_moves     (Move::List& moves, const Position& pos);

    static void gen_nbrq_moves     (Move::List& moves, const Position& pos, int dest, const bitset<128>& pins);
    
    void init()
    {
        type88[128 + 16 - 1] |= Piece::WhitePawnFlag256;
        type88[128 + 16 + 1] |= Piece::WhitePawnFlag256;
        type88[128 - 16 - 1] |= Piece::BlackPawnFlag256;
        type88[128 - 16 + 1] |= Piece::BlackPawnFlag256;

        for (auto inc : KnightIncs) {
            inc88[128 + inc] = inc;
            type88[128 + inc] |= Piece::KnightFlag256;
        }

        for (auto inc : BishopIncs) {
            for (int i = 1; i <= 7; i++) {
                inc88[128 + inc * i] = inc;
                type88[128 + inc * i] |= Piece::BishopFlag256;
            }
        }

        for (auto inc : RookIncs) {
            for (int i = 1; i <= 7; i++) {
                inc88[128 + inc * i] = inc;
                type88[128 + inc * i] |= Piece::RookFlag256;
            }
        }

        for (auto inc : QueenIncs)
            type88[128 + inc] |= Piece::KingFlag256;

        for (int i = 0; i < 256; i++)
            castle88[i] = ~0;

        castle88[A1] = ~Position::WhiteCastleQFlag;
        castle88[E1] = ~(Position::WhiteCastleQFlag | Position::WhiteCastleKFlag);
        castle88[H1] = ~Position::WhiteCastleKFlag;

        castle88[A8] = ~Position::BlackCastleQFlag;
        castle88[E8] = ~(Position::BlackCastleQFlag | Position::BlackCastleKFlag);
        castle88[H8] = ~Position::BlackCastleKFlag;
    }

    size_t gen_pseudo_moves(Move::List& moves, const Position& pos)
    {
        //assert(pos.is_ok() == 0);

        const int side = pos.side();

        for (auto orig : pos.piece_list(Piece::PieceList12[side][Piece::Pawn]))
            gen_pawn_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::PieceList12[side][Piece::Knight]))
            gen_knight_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::PieceList12[side][Piece::Bishop]))
            gen_bishop_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::PieceList12[side][Piece::Rook]))
            gen_rook_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::PieceList12[side][Piece::Queen]))
            gen_queen_moves(moves, pos, orig);

        {
            gen_king_moves(moves, pos);

            if (!pos.last_move().is_check())
                gen_king_castles(moves, pos);
        }

        return moves.size();
    }

    //size_t gen_legal_moves(Move::List& moves, const Position& pos)
    size_t gen_legal_moves(Move::List& moves, const Position&)
    {
        return moves.size();
    }

    void gen_pawn_moves(Move::List& moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Pawn));

        Piece::Piece256 oflag = Piece::BlackFlag256 >> pos.side();

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
                    moves.add(Move(orig, dest) | Gen::Move::DoubleFlag);
            }
        }
    }

    void gen_knight_moves(Move::List& moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Knight));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : KnightIncs) {
            int dest = orig;

            piece = pos[dest += inc];

            if (piece == Piece::PieceNone256)
                moves.add(Move(orig, dest));
            else if (piece & ocolor)
                moves.add(Move(orig, dest, piece));
        }
    }

    void gen_bishop_moves(Move::List& moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Bishop));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : BishopIncs) {
            int dest = orig;

            while ((piece = pos[dest += inc]) == Piece::PieceNone256)
                moves.add(Move(orig, dest));

            if (piece & ocolor)
                moves.add(Move(orig, dest, piece));
        }
    }

    void gen_rook_moves(Move::List& moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Rook));
        
        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : RookIncs) {
            int dest = orig;

            while ((piece = pos[dest += inc]) == Piece::PieceNone256)
                moves.add(Move(orig, dest));

            if (piece & ocolor)
                moves.add(Move(orig, dest, piece));
        }
    }

    void gen_queen_moves(Move::List& moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Queen));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : QueenIncs) {
            int dest = orig;

            while ((piece = pos[dest += inc]) == Piece::PieceNone256)
                moves.add(Move(orig, dest));

            if (piece & ocolor)
                moves.add(Move(orig, dest, piece));
        }
    }

    void gen_king_moves(Move::List& moves, const Position& pos)
    {
        int king = pos.king_sq();

        assert(is_sq88(king));
        assert(pos.is_me(king));
        assert(pos.is_piece(king, Piece::King));
        
        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : QueenIncs) {
            int dest = king;

            piece = pos[dest += inc];

            if (piece == Piece::PieceNone256)
                moves.add(Move(king, dest));
            else if (piece & ocolor)
                moves.add(Move(king, dest, piece));
        }
    }

    size_t gen_king_evasions(Move::List& moves, const Position& pos)
    {
        const Move& lm = pos.last_move();
        
        assert(lm.is_check());

        const int king = pos.king_sq();
        const int mside = pos.side();
        const int oside = Piece::flip_side(mside);

        assert(pos.side_attacks(oside, king));
        
        Piece::Piece256 mflag = Piece::make_flag(mside);

        if (lm.is_double_check()) {
            assert(lm.is_dir_rev_check() || lm.is_rev_rev_check());

            const int inc_direct = Piece::is_pawn(lm.dest()) ? 0 : delta_inc(lm.dest(), king);
            const int inc_reveal = delta_inc(lm.orig(), king);

            for (auto inc : QueenIncs) {
                if (inc == inc_direct || inc == inc_reveal)
                    continue;

                const int dest = king + inc;

                if (!is_sq88(dest))
                    continue;
                if (pos[dest] & mflag)
                    continue;
                if (!pos.side_attacks(oside, dest))
                    moves.add(Move(king, dest, pos[dest]));
            }

            return moves.size();
        }

        assert(!lm.is_double_check());

        int dest;
        int inc;

        if (lm.is_dir_check()) {
            dest = lm.is_castle() ? (lm.orig() + lm.dest()) >> 1 : lm.dest();

            assert(is_sq88(dest));
            assert(pos.is_op(dest));
            assert(!lm.is_castle() || Piece::is_rook(pos[dest]));

            inc = delta_inc(dest, king);
            
            assert(inc != 0);
        }
        else {
            inc = delta_inc(lm.orig(), king);

            if (inc == 0)
                cout << hex << uint32_t(lm) << endl;

            assert(inc != 0);

            dest = lm.orig();

            do { dest -= inc; } while (pos[dest] == Piece::PieceNone256);

            assert(is_sq88(dest));
            assert(pos.is_op(dest));
        }
       
        // TODO: is passing bitset reference faster then returning a bitset?
        bitset<128> pins;

        pos.mark_pins(pins);
        
        const Piece::Piece256 mpawn = Piece::make_pawn(mside);
        
        const int pinc = pawn_inc(mside);

        int orig;

        // promotions
        
        if (sq88_rank(dest, mside) == Rank8) {
            orig = dest - pinc - 1;

            //if (pos[orig] == mpawn && !pins.test(orig)) {
            if (pos[orig] == mpawn) {
                Move m(orig, dest, pos[dest]);

                moves.add(m | Move::PromoKnightFlag);
                moves.add(m | Move::PromoBishopFlag);
                moves.add(m | Move::PromoRookFlag);
                moves.add(m | Move::PromoQueenFlag);
            }

            orig = dest - pinc + 1;

            //if (pos[orig] == mpawn && !pins.test(orig)) {
            if (pos[orig] == mpawn) {
                Move m(orig, dest, pos[dest]);

                moves.add(m | Move::PromoKnightFlag);
                moves.add(m | Move::PromoBishopFlag);
                moves.add(m | Move::PromoRookFlag);
                moves.add(m | Move::PromoQueenFlag);
            }
        }
        else {
            orig = dest - pinc - 1;

            //if (pos[orig] == mpawn && !pins.test(orig))
            if (pos[orig] == mpawn)
                moves.add(Move(orig, dest, pos[dest]));

            orig = dest - pinc + 1;

            //if (pos[orig] == mpawn && !pins.test(orig))
            if (pos[orig] == mpawn)
                moves.add(Move(orig, dest, pos[dest]));
        }

        if (lm.is_double() && lm.is_dir_check()) {
            // TODO: does this make more sense than using pos.ep_sq()?
            int ep_sq = lm.dest() + pinc;

            orig = dest - 1;

            //if (pos[orig] == mpawn && !pins.test(orig)) {
            if (pos[orig] == mpawn) {
                //if (is_pinned(king, orig, ep_sq))
                
                moves.add(Move(orig, ep_sq, pos[lm.dest()]) | Move::EPFlag);
            }

            orig = dest + 1;
            
            //if (pos[orig] == mpawn && !pins.test(orig)) {
            if (pos[orig] == mpawn) {
                //if (is_pinned(king, orig, ep_sq))
 
                moves.add(Move(orig, ep_sq, pos[lm.dest()]) | Move::EPFlag);
            }
        }

        gen_nbrq_moves(moves, pos, dest, pins);

        while ((dest += inc) != king) {
            orig = dest - pinc;

            if (pos[orig] == mpawn) {
                Move m(orig, dest);

                if (sq88_rank(dest, mside) == Rank8) {
                    moves.add(m | Move::PromoKnightFlag);
                    moves.add(m | Move::PromoBishopFlag);
                    moves.add(m | Move::PromoRookFlag);
                    moves.add(m | Move::PromoQueenFlag);
                }
                else
                    moves.add(m);
            }

            if (pos[orig] == Piece::PieceNone256) {
                if (sq88_rank(dest, mside) == Rank4) {
                    orig -= pinc;

                    if (pos[orig] == mpawn)
                        moves.add(Move(orig, dest) | Move::DoubleFlag);
                }
            }

            gen_nbrq_moves(moves, pos, dest, pins);
        }

        if (lm.is_dir_check() && Piece::is_pawn(pos[lm.dest()]))
            inc = 0;

        for (auto ki : QueenIncs) {
            if (ki == inc)
                continue;

            dest = king + ki;

            if (!is_sq88(dest))
                continue;
            if (pos[dest] & mflag)
                continue;
            if (!pos.side_attacks(oside, dest))
                moves.add(Move(king, dest, pos[dest]));
        }

        return moves.size();
    }

    void gen_king_castles(Move::List& moves, const Position& pos)
    {
        int king = pos.king_sq();

        assert(is_sq88(king));
        assert(pos.is_me(king));
        assert(pos.is_piece(king, Piece::King));
        
        if (pos.can_castle_k()) {
            if (   pos.is_empty(king + 1)
                && pos.is_empty(king + 2)) {

                bool check = pos.side_attacks(Piece::flip_side(pos.side()), king + 1)
                          || pos.side_attacks(Piece::flip_side(pos.side()), king + 2);

                if (!check)
                    moves.add(Move(king, king + 2) | Gen::Move::CastleFlag);
            }
        }
        if (pos.can_castle_q()) {
            if (   pos.is_empty(king - 1)
                && pos.is_empty(king - 2)
                && pos.is_empty(king - 3)) {

                bool check = pos.side_attacks(Piece::flip_side(pos.side()), king - 1)
                          || pos.side_attacks(Piece::flip_side(pos.side()), king - 2);

                if (!check)
                    moves.add(Move(king, king - 2) | Gen::Move::CastleFlag);
            }
        }
    }

    void gen_nbrq_moves(Move::List& moves, const Position& pos, int dest, const bitset<128>& pins)
    {
        const int mside = pos.side();

        for (const int orig : pos.piece_list(Piece::WhiteKnight12 + mside)) {
            assert(is_sq88(orig));

            if (pins.test(orig))
                continue;

            if (delta_type(orig, dest) & Piece::KnightFlag256)
                moves.add(Move(orig, dest, pos[dest]));
        }

        for (const int orig : pos.piece_list(Piece::WhiteBishop12 + mside)) {
            assert(is_sq88(orig));

            if (pins.test(orig))
                continue;

            if ((delta_type(orig, dest) & Piece::BishopFlag256) == 0)
                continue;

            if (int inc = delta_inc(dest, orig); inc != 0) {
                int sq = dest;

                do { sq += inc; } while (pos[sq] == Piece::PieceNone256);

                if (sq == orig)
                    moves.add(Move(orig, dest, pos[dest]));
            }
        }

        for (const int orig : pos.piece_list(Piece::WhiteRook12 + mside)) {
            assert(is_sq88(orig));

            if (pins.test(orig))
                continue;

            if ((delta_type(orig, dest) & Piece::RookFlag256) == 0)
                continue;

            if (int inc = delta_inc(dest, orig); inc != 0) {
                int sq = dest;

                do { sq += inc; } while (pos[sq] == Piece::PieceNone256);

                if (sq == orig)
                    moves.add(Move(orig, dest, pos[dest]));
            }
        }

        for (const int orig : pos.piece_list(Piece::WhiteQueen12 + mside)) {
            assert(is_sq88(orig));

            if (pins.test(orig))
                continue;

            if ((delta_type(orig, dest) & Piece::QueenFlags256) == 0)
                continue;

            if (int inc = delta_inc(dest, orig); inc != 0) {
                int sq = dest;

                do { sq += inc; } while (pos[sq] == Piece::PieceNone256);

                if (sq == orig)
                    moves.add(Move(orig, dest, pos[dest]));
            }
        }
    }

    int delta_inc(int orig, int dest)
    {
        assert(is_sq88(orig));
        assert(is_sq88(dest));

        return inc88[128 + dest - orig];
    }

    Piece::Piece256 delta_type(int orig, int dest)
    {
        assert(is_sq88(orig));
        assert(is_sq88(dest));

        return type88[128 + dest - orig];
    }

    int castle_flag(int sq)
    {
        assert(is_sq88(sq));

        return castle88[sq];
    }
}
