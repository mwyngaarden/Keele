#include <iostream>
#include "gen.h"
#include "move.h"
#include "piece.h"
#include "pos.h"
using namespace std;

namespace Gen {

    static int dir88[256];
    static int flag88[256];

    static int castle88[256];

    static Move* add_pawn_moves     (Move* moves, const Position& pos, int orig);
    static Move* add_knight_moves   (Move* moves, const Position& pos, int orig);
    static Move* add_bishop_moves   (Move* moves, const Position& pos, int orig);
    static Move* add_rook_moves     (Move* moves, const Position& pos, int orig);
    static Move* add_queen_moves    (Move* moves, const Position& pos, int orig);
    static Move* add_king_moves     (Move* moves, const Position& pos, int orig);
    
    void init()
    {
        flag88[128 + 16 - 1] |= Piece::WhitePawnFlag256;
        flag88[128 + 16 + 1] |= Piece::WhitePawnFlag256;
        flag88[128 - 16 - 1] |= Piece::BlackPawnFlag256;
        flag88[128 - 16 + 1] |= Piece::BlackPawnFlag256;

        for (auto dir : piece_dirs[Piece::Knight]) {
            dir88[128 + dir] = dir;
            flag88[128 + dir] |= Piece::KnightFlag256;
        }

        for (auto dir : piece_dirs[Piece::Bishop]) {
            for (int i = 1; i <= 7; i++) {
                dir88[128 + dir * i] = dir;
                flag88[128 + dir * i] |= Piece::BishopFlag256;
            }
        }

        for (auto dir : piece_dirs[Piece::Rook]) {
            for (int i = 1; i <= 7; i++) {
                dir88[128 + dir * i] = dir;
                flag88[128 + dir * i] |= Piece::RookFlag256;
            }
        }

        for (auto dir : piece_dirs[Piece::Queen])
            flag88[128 + dir] |= Piece::KingFlag256;

        for (int i = 0; i < 256; i++)
            castle88[i] = ~0;

        castle88[A1] = ~Position::WhiteCastleQFlag;
        castle88[E1] = ~(Position::WhiteCastleQFlag | Position::WhiteCastleKFlag);
        castle88[H1] = ~Position::WhiteCastleKFlag;

        castle88[A8] = ~Position::BlackCastleQFlag;
        castle88[E8] = ~(Position::BlackCastleQFlag | Position::BlackCastleKFlag);
        castle88[H8] = ~Position::BlackCastleKFlag;
    }

    Move* add_pseudo_moves(Move* moves, const Position& pos)
    {
        assert(moves != nullptr);
        assert(pos.is_ok() == 0);

        int side = pos.side();

        for (auto orig : pos.piece_list(side, Piece::Pawn))
            moves = add_pawn_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(side, Piece::Knight))
            moves = add_knight_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(side, Piece::Bishop))
            moves = add_bishop_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(side, Piece::Rook))
            moves = add_rook_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(side, Piece::Queen))
            moves = add_queen_moves(moves, pos, orig);

        for (auto orig : pos.piece_list(side, Piece::King))
            moves = add_king_moves(moves, pos, orig);

        return moves;
    }

    Move* add_evasion_moves(Move* moves, const Position& pos)
    {


        return moves;
    }

    Move* add_legal_moves(Move* moves, const Position& pos)
    {


        return moves;
    }

    Move* add_pawn_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Pawn));

        Piece::Piece256 oflag = Piece::BlackFlag256 >> pos.side();

        int inc = pawn_inc(pos.side());

        int rel_rank = sq88_rank(orig, pos.side());

        int dest;

        assert(rel_rank >= Rank2 && rel_rank <= Rank7);

        // promotions

        if (rel_rank == Rank7) {
            dest = orig + inc - 1;

            if (pos[dest] & oflag) {
                Move m(orig, dest, pos[dest]);

                *moves++ = m | Move::PromoKnightFlag;
                *moves++ = m | Move::PromoBishopFlag;
                *moves++ = m | Move::PromoRookFlag;
                *moves++ = m | Move::PromoQueenFlag;
            }

            dest += 2;

            if (pos[dest] & oflag) {
                Move m(orig, dest, pos[dest]);

                *moves++ = m | Move::PromoKnightFlag;
                *moves++ = m | Move::PromoBishopFlag;
                *moves++ = m | Move::PromoRookFlag;
                *moves++ = m | Move::PromoQueenFlag;
            }

            // single push

            dest -= 1;

            if (pos.is_empty(dest)) {
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

            dest = orig + inc - 1;
            
            if (pos[dest] & oflag)
                *moves++ = Move(orig, dest, pos[dest]);

            dest += 2;

            if (pos[dest] & oflag)
                *moves++ = Move(orig, dest, pos[dest]);

            // single push
            
            dest -= 1;

            if (pos.is_empty(dest)) {
                *moves++ = Move(orig, dest);

                // double push

                dest += inc;

                if (rel_rank == Rank2 && pos.is_empty(dest))
                    *moves++ = Move(orig, dest) | Gen::Move::DoubleFlag;
            }
        }

        return moves;
    }

    Move* add_knight_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Knight));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto dir : piece_dirs[Piece::Knight]) {
            int dest = orig;

            piece = pos[dest += dir];

            if (piece == Piece::PieceNone256)
                *moves++ = Move(orig, dest);
            else if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* add_bishop_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Bishop));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto dir : piece_dirs[Piece::Bishop]) {
            int dest = orig;

            while ((piece = pos[dest += dir]) == Piece::PieceNone256)
                *moves++ = Move(orig, dest);

            if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* add_rook_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Rook));
        
        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto dir : piece_dirs[Piece::Rook]) {
            int dest = orig;

            while ((piece = pos[dest += dir]) == Piece::PieceNone256)
                *moves++ = Move(orig, dest);

            if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* add_queen_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Queen));

        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto dir : piece_dirs[Piece::Queen]) {
            int dest = orig;

            while ((piece = pos[dest += dir]) == Piece::PieceNone256)
                *moves++ = Move(orig, dest);

            if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        return moves;
    }

    Move* add_king_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::King));
        
        Piece::Piece256 ocolor = Piece::BlackFlag256 >> pos.side();
        Piece::Piece256 piece;

        for (auto dir : piece_dirs[Piece::King]) {
            int dest = orig;

            piece = pos[dest += dir];

            if (piece == Piece::PieceNone256)
                *moves++ = Move(orig, dest);
            else if (piece & ocolor)
                *moves++ = Move(orig, dest, piece);
        }

        int king = pos.king_sq();

        bool can_castle = pos.can_castle() && !pos.side_attacks(Piece::flip_side(pos.side()), king);

        if (can_castle) {
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
        }

        return moves;
    }

    int move_dir(int orig, int dest)
    {
        assert(is_sq88(orig));
        assert(is_sq88(dest));

        return dir88[128 + dest - orig];
    }

    Piece::Piece256 move_flag(int orig, int dest)
    {
        assert(is_sq88(orig));
        assert(is_sq88(dest));

        return flag88[128 + dest - orig];
    }

    // FIXME: slow
    int castle_flag(int orig)
    {
        assert(is_sq88(orig));

        return castle88[orig];
    }
}
