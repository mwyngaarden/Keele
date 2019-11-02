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

        for (int i = 0; i < pos.get_pcount(side, Piece::Pawn); i++)
            moves = add_pawn_moves(moves, pos, pos.get_psq(side, Piece::Pawn, i));

        for (int i = 0; i < pos.get_pcount(side, Piece::Knight); i++)
            moves = add_knight_moves(moves, pos, pos.get_psq(side, Piece::Knight, i));

        for (int i = 0; i < pos.get_pcount(side, Piece::Bishop); i++)
            moves = add_bishop_moves(moves, pos, pos.get_psq(side, Piece::Bishop, i));

        for (int i = 0; i < pos.get_pcount(side, Piece::Rook); i++)
            moves = add_rook_moves(moves, pos, pos.get_psq(side, Piece::Rook, i));

        for (int i = 0; i < pos.get_pcount(side, Piece::Queen); i++)
            moves = add_queen_moves(moves, pos, pos.get_psq(side, Piece::Queen, i));

        for (int i = 0; i < pos.get_pcount(side, Piece::King); i++)
            moves = add_king_moves(moves, pos, pos.get_psq(side, Piece::King, i));

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

        Piece::Piece256 mflag = Piece::WhiteFlag256 << pos.side();
        Piece::Piece256 oflag = Piece::BlackFlag256 >> pos.side();

        int inc = pawn_inc(pos.side());

        int mfile = sq88_file(orig);

        int abs_rank = sq88_rank(orig);
        int rel_rank = sq88_rank(orig, pos.side());

        assert(abs_rank >= Rank2 && abs_rank <= Rank7);
        assert(rel_rank >= Rank2 && rel_rank <= Rank7);

        // promotions

        if (rel_rank == Rank7) {
            if (int dest = orig + inc - 1; pos.square(dest) & oflag) {
                Move m(orig, dest, pos.square(dest));

                *moves++ = m | Move::PromoKnightFlag;
                *moves++ = m | Move::PromoBishopFlag;
                *moves++ = m | Move::PromoRookFlag;
                *moves++ = m | Move::PromoQueenFlag;
            }
            if (int dest = orig + inc + 1; pos.square(dest) & oflag) {
                Move m(orig, dest, pos.square(dest));

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
                if (abs(mfile - sq88_file(ep_sq)) == 1)
                    *moves++ = Move(orig, pos.ep_sq(), pos.square(ep_sq - inc)) | Move::EPFlag;
            }
            
            if (int dest = orig + inc - 1; pos.square(dest) & oflag)
                *moves++ = Move(orig, dest, pos.square(dest));

            if (int dest = orig + inc + 1; pos.square(dest) & oflag)
                *moves++ = Move(orig, dest, pos.square(dest));

            // single push

            if (int dest = orig + inc; pos.is_empty(dest)) {
                *moves++ = Move(orig, dest);

                // double push

                if (rel_rank == Rank2 && pos.is_empty(dest += inc))
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

        for (auto dir : piece_dirs[Piece::Knight]) {
            int dest = orig + dir;

            if (is_sq88(dest)) {
                if (pos.is_empty(dest))
                    *moves++ = Move(orig, dest);
                else if (pos.is_op(dest))
                    *moves++ = Move(orig, dest, pos.square(dest));
            }
        }

        return moves;
    }

    Move* add_bishop_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Bishop));

        Piece::Piece256 ecolor = Piece::BlackFlag256 >> pos.side();

        for (auto dir : piece_dirs[Piece::Bishop]) {
            for (int dest = orig + dir; is_sq88(dest); dest += dir) {
                if (pos.is_empty(dest))
                    *moves++ = Move(orig, dest);
                else {
                    if (pos.is_op(dest))
                        *moves++ = Move(orig, dest, pos.square(dest));

                    break;
                }
            }
        }

        return moves;
    }

    Move* add_rook_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Rook));

        for (auto dir : piece_dirs[Piece::Rook]) {
            for (int dest = orig + dir; is_sq88(dest); dest += dir) {
                if (pos.is_empty(dest))
                    *moves++ = Move(orig, dest);
                else {
                    if (pos.is_op(dest))
                        *moves++ = Move(orig, dest, pos.square(dest));

                    break;
                }
            }
        }

        return moves;
    }

    Move* add_queen_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::Queen));

        for (auto dir : piece_dirs[Piece::Queen]) {
            for (int dest = orig + dir; is_sq88(dest); dest += dir) {
                if (pos.is_empty(dest))
                    *moves++ = Move(orig, dest);
                else {
                    if (pos.is_op(dest))
                        *moves++ = Move(orig, dest, pos.square(dest));

                    break;
                }
            }
        }

        return moves;
    }

    Move* add_king_moves(Move* moves, const Position& pos, int orig)
    {
        assert(is_sq88(orig));
        assert(pos.is_me(orig));
        assert(pos.is_piece(orig, Piece::King));

        for (auto dir : piece_dirs[Piece::King]) {
            int dest = orig + dir;

            if (is_sq88(dest)) {
                if (pos.is_empty(dest))
                    *moves++ = Move(orig, dest);
                else if (pos.is_op(dest))
                    *moves++ = Move(orig, dest, pos.square(dest));
            }
        }

        int king = pos.king_sq();

        bool gen_castle = pos.can_castle() && !pos.side_attacks(Piece::flip_side(pos.side()), king);

        if (gen_castle) {
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
