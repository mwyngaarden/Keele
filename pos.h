#ifndef POS_H
#define POS_H

#include <string>
#include <cassert>
#include <cstdint>
#include "gen.h"
#include "move.h"
#include "piece.h"
#include "square.h"


// TODO: add constructor taking fen and make class immutable
class Position {

public:

    static constexpr char StartPos[] = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    static constexpr int WhiteCastleKFlag   = 1 << 0;
    static constexpr int BlackCastleKFlag   = 1 << 1;
    static constexpr int WhiteCastleQFlag   = 1 << 2;
    static constexpr int BlackCastleQFlag   = 1 << 3;

    static constexpr int WhiteCastleFlags   = WhiteCastleQFlag | WhiteCastleKFlag;
    static constexpr int BlackCastleFlags   = BlackCastleQFlag | BlackCastleKFlag;

    static constexpr int CastleFlags        = BlackCastleQFlag | WhiteCastleQFlag
                                            | BlackCastleKFlag | WhiteCastleKFlag;

    Position(const std::string& fen = StartPos);

    void init();

    std::string get_fen() const;

    int is_ok(bool incheck = true) const;

    void   make_move(const Gen::Move& move,       Gen::Undo& undo);
    void unmake_move(const Gen::Move& move, const Gen::Undo& undo);

    void add_piece(int sq, Piece::Piece256 p);
    void rem_piece(int sq);
    void move_piece(int orig, int dest);

    bool side_attacks(int side, int sq) const;
    
    inline const Piece::Piece256& square(int sq) const
    {
        assert(sq >= -48 && sq < 176);

        return square_[48 + sq];
    }

    inline bool is_empty(int sq) const
    {
        assert(is_sq88(sq));

        return square(sq) == Piece::PieceNone256;
    }

    inline bool is_occupied(int sq) const
    {
        assert(is_sq88(sq));

        return square(sq) != Piece::PieceNone256;
    }

    inline bool is_op(int sq) const
    {
        assert(is_sq88(sq));

        return square(sq) & (Piece::BlackFlag256 >> side_);
    }

    inline bool is_me(int sq) const
    {
        assert(is_sq88(sq));

        return square(sq) & (Piece::WhiteFlag256 << side_);
    }

    inline bool is_piece(int sq, int piece) const
    {
        assert(is_sq88(sq));
        assert(Piece::piece_is_ok(piece));

        return Piece::to_piece(square(sq)) == piece;
    }

    inline const Piece::PieceList& piece_list(int side, int piece) const
    {
        assert(Piece::side_is_ok(side));
        assert(Piece::piece_is_ok(piece));

        return pieces_[side][piece];
    }

    inline int get_psq(int side, int piece, int index) const
    {
        assert(Piece::side_is_ok(side));
        assert(Piece::piece_is_ok(piece));

        return pieces_[side][piece][index];
    }

    inline int king_sq(int side) const
    {
        assert(Piece::side_is_ok(side));

        return pieces_[side][Piece::King][0];
    }

    inline int king_sq() const
    {
        return king_sq(side_);
    }

    inline int side()                      const { return side_; }
    inline int ep_sq()                     const { return ep_sq_; }
    inline int half_moves()                const { return half_moves_; }
    inline int full_moves()                const { return full_moves_; }

    inline bool can_castle_k(int side) const { return (flags_ & (WhiteCastleKFlag << side)) != 0; }
    inline bool can_castle_q(int side) const { return (flags_ & (WhiteCastleQFlag << side)) != 0; }
    inline bool can_castle  (int side) const { return (flags_ & (WhiteCastleFlags << side)) != 0; }
    inline bool can_castle_k()         const { return can_castle_k(side_); }
    inline bool can_castle_q()         const { return can_castle_q(side_); }
    inline bool can_castle  ()         const { return can_castle(side_); }

    // misc
    
    std::string dump() const;

    inline void add_check(int sq)
    {
        assert(is_sq88(sq));
        assert(check_sq_[1] == SquareNone);

        check_sq_[1] = check_sq_[0];
        check_sq_[0] = sq;
    }


private:
    inline Piece::Piece256& square(int sq)
    {
        assert(sq >= -48 && sq < 176);

        return square_[48 + sq];
    }
    
    void set_checks();
    void set_checks(const Gen::Move& last_move);
    
    inline void rem_checks()
    {
        check_sq_[0] = SquareNone;
        check_sq_[1] = SquareNone;
    }

    Piece::Piece256 square_[16 * 14];

    int side_ = -1;
    int flags_ = 0;
    int ep_sq_ = SquareNone;
    int half_moves_ = 0;
    int full_moves_ = 1;
    
    int check_sq_[2];
    
    Gen::Move last_move_ = 0;

    Piece::PieceList pieces_[2][6];

};

#endif
