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

    static Move* gen_pawn_moves     (Move* moves, const Position& pos, int orig);
    static Move* gen_knight_moves   (Move* moves, const Position& pos, int orig);
    static Move* gen_bishop_moves   (Move* moves, const Position& pos, int orig);
    static Move* gen_rook_moves     (Move* moves, const Position& pos, int orig);
    static Move* gen_queen_moves    (Move* moves, const Position& pos, int orig);

    static Move* gen_king_moves     (Move* moves, const Position& pos, int orig);
    static Move* gen_king_evasions  (Move* moves, const Position& pos, int orig);
    static Move* gen_king_castles   (Move* moves, const Position& pos, int orig);
    
    void init()
    {
        type88[128 + 16 - 1] |= Piece::WhitePawnFlag256;
        type88[128 + 16 + 1] |= Piece::WhitePawnFlag256;
        type88[128 - 16 - 1] |= Piece::BlackPawnFlag256;
        type88[128 - 16 + 1] |= Piece::BlackPawnFlag256;

        for (auto inc : piece_incs[Piece::Knight]) {
            inc88[128 + inc] = inc;
            type88[128 + inc] |= Piece::KnightFlag256;
        }

        for (auto inc : piece_incs[Piece::Bishop]) {
            for (int i = 1; i <= 7; i++) {
                inc88[128 + inc * i] = inc;
                type88[128 + inc * i] |= Piece::BishopFlag256;
            }
        }

        for (auto inc : piece_incs[Piece::Rook]) {
            for (int i = 1; i <= 7; i++) {
                inc88[128 + inc * i] = inc;
                type88[128 + inc * i] |= Piece::RookFlag256;
            }
        }

        for (auto inc : piece_incs[Piece::Queen])
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

    Move* gen_pseudo_moves(Move* moves, const Position& pos)
    {
        assert(moves != nullptr);
        assert(pos.is_ok() == 0);

        int side = pos.side();
        int king = pos.king_sq();

        for (auto orig : pos.piece_list(Piece::WhitePawn12 + side))
            moves = gen_pawn_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::WhiteKnight12 + side))
            moves = gen_knight_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::WhiteBishop12 + side))
            moves = gen_bishop_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::WhiteRook12 + side))
            moves = gen_rook_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(Piece::WhiteQueen12 + side))
            moves = gen_queen_moves(moves, pos, orig);

        moves = gen_king_moves(moves, pos, king);

        if (pos.can_castle()) {
            bool in_check = pos.side_attacks(Piece::flip_side(side), king);

            if (!in_check)
                moves = gen_king_castles(moves, pos, king);
        }

        return moves;
    }

    Move* gen_legal_moves(Move* moves, const Position& pos)
    {
        return moves;
    }

    Move* gen_pawn_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Pawn));

        Piece::Piece256 oflag = Piece::BlackFlag256 >> pos.side();

        int inc = pawn_inc(pos.side());

        int rel_rank = sq88_rank(orig, pos.side());

        assert(rel_rank >= Rank2 && rel_rank <= Rank7);

        // promotions

        if (rel_rank == Rank7) {
            if (int dest = orig + inc - 1; pos[dest] & oflag) {
                Move m(orig, dest, pos[dest]);

                *moves++ = m | Move::PromoKnightFlag;
                *moves++ = m | Move::PromoBishopFlag;
                *moves++ = m | Move::PromoRookFlag;
                *moves++ = m | Move::PromoQueenFlag;
            }

            if (int dest = orig + inc + 1; pos[dest] & oflag) {
                Move m(orig, dest, pos[dest]);

                *moves++ = m | Move::PromoKnightFlag;
                *moves++ = m | Move::PromoBishopFlag;
                *moves++ = m | Move::PromoRookFlag;
                *moves++ = m | Move::PromoQueenFlag;
            }

            if (int dest = orig + inc; pos.is_empty(dest)) {
                Move m(orig, dest);

                *moves++ = m | Move::PromoKnightFlag;
                *moves++ = m | Move::PromoBishopFlag;
                *moves++ = m | Move::PromoRookFlag;
                *moves++ = m | Move::PromoQueenFlag;
            }
        }
        else {
            if (int ep_sq = pos.ep_sq(); rel_rank == Rank5 && ep_sq != SquareNone) {
                if (abs(sq88_file(orig) - sq88_file(ep_sq)) == 1)
                    *moves++ = Move(orig, pos.ep_sq(), pos[ep_sq - inc]) | Move::EPFlag;
            }

            if (int dest = orig + inc - 1; pos[dest] & oflag)
                *moves++ = Move(orig, dest, pos[dest]);

            if (int dest = orig + inc + 1; pos[dest] & oflag)
                *moves++ = Move(orig, dest, pos[dest]);

            // single push
            
            if (int dest = orig + inc; pos.is_empty(dest)) {
                *moves++ = Move(orig, dest);

                // double push

                dest += inc;

                if (rel_rank == Rank2 && pos.is_empty(dest))
                    *moves++ = Move(orig, dest) | Gen::Move::DoubleFlag;
            }
        }

        return moves;
    }

    Move* gen_knight_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Knight));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : piece_incs[Piece::Knight]) {
            int dest = orig;

            piece = pos[dest += inc];

            if (piece == Piece::PieceNone256)
                *moves++ = Move(orig, dest);
            else if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* gen_bishop_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Bishop));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : piece_incs[Piece::Bishop]) {
            int dest = orig;

            while ((piece = pos[dest += inc]) == Piece::PieceNone256)
                *moves++ = Move(orig, dest);

            if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* gen_rook_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Rook));
        
        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : piece_incs[Piece::Rook]) {
            int dest = orig;

            while ((piece = pos[dest += inc]) == Piece::PieceNone256)
                *moves++ = Move(orig, dest);

            if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* gen_queen_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Queen));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : piece_incs[Piece::Queen]) {
            int dest = orig;

            while ((piece = pos[dest += inc]) == Piece::PieceNone256)
                *moves++ = Move(orig, dest);

            if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* gen_king_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::King));
        
        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto inc : piece_incs[Piece::King]) {
            int dest = orig;

            piece = pos[dest += inc];

            if (piece == Piece::PieceNone256)
                *moves++ = Move(orig, dest);
            else if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        //if (pos.can_castle())
            //moves = gen_king_castles(moves, pos, orig);

        return moves;
    }

    Move* gen_king_evasions(Move* moves, const Position& pos, int orig)
    {
        return moves;
    }

    Move* gen_king_castles(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::King));
        
        if (pos.can_castle_k()) {
            if (pos.is_empty(orig + 1) && pos.is_empty(orig + 2)) {
                bool check = pos.side_attacks(Piece::flip_side(pos.side()), orig + 1)
                          || pos.side_attacks(Piece::flip_side(pos.side()), orig + 2);

                if (!check)
                    *moves++ = Move(orig, orig + 2) | Gen::Move::CastleFlag;
            }
        }
        if (pos.can_castle_q()) {
            if (pos.is_empty(orig - 1) && pos.is_empty(orig - 2) && pos.is_empty(orig - 3)) {
                bool check = pos.side_attacks(Piece::flip_side(pos.side()), orig - 1)
                          || pos.side_attacks(Piece::flip_side(pos.side()), orig - 2);

                if (!check)
                    *moves++ = Move(orig, orig - 2) | Gen::Move::CastleFlag;
            }
        }

        return moves;
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

    // FIXME: slow
    int castle_flag(int orig)
    {
        assert(is_sq88(orig));

        return castle88[orig];
    }
}
