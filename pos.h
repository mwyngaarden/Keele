#ifndef POS_H
#define POS_H

#include <string>
#include <cassert>
#include <cstdint>
#include "gen.h"
#include "hash.h"
#include "move.h"
#include "piece.h"
#include "square.h"

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

    std::string to_fen() const;

    int is_ok(bool in_check = true) const;

    bool move_was_legal() const;

    void   note_move(Gen::Move& move) const;

    void   make_move(const Gen::Move& move,       Gen::Undo& undo);
    void unmake_move(const Gen::Move& move, const Gen::Undo& undo);

    uint64_t      key() const { return key_; }
    uint64_t calc_key() const;

    void mark_pins();

    void add_piece(int sq, Piece::Piece256 p);
    void rem_piece(int sq);
    void mov_piece(int orig, int dest);

    bool ep_is_valid() const;

    bool side_attacks(int side, int dest) const;
    bool piece_attacks(int orig, int dest) const;

    inline const Piece::Piece256& operator[](int sq) const { return square(sq); }
    
    inline const Piece::Piece256& square(int sq) const
    {
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
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

    inline const Piece::List& piece_list(int p12) const
    {
        assert(Piece::piece12_is_ok(p12));

        return piece_list_[p12];
    }

    inline int king_sq(int side) const
    {
        assert(Piece::side_is_ok(side));

        return piece_list_[Piece::WhiteKing12 + side][0];
    }

    inline int king_sq() const
    {
        return king_sq(side_);
    }

    inline int side()                  const { return side_; }
    inline int ep_sq()                 const { return ep_sq_; }
    inline int half_moves()            const { return half_moves_; }
    inline int full_moves()            const { return full_moves_; }

    inline bool can_castle_k(int side) const { return (flags_ & (WhiteCastleKFlag << side)) != 0; }
    inline bool can_castle_q(int side) const { return (flags_ & (WhiteCastleQFlag << side)) != 0; }
    inline bool can_castle  (int side) const { return (flags_ & (WhiteCastleFlags << side)) != 0; }
    inline bool can_castle_k()         const { return can_castle_k(side_); }
    inline bool can_castle_q()         const { return can_castle_q(side_); }
    inline bool can_castle  ()         const { return can_castle(side_); }

    // misc
    
    std::string dump() const;

    inline const Gen::Move& last_move() const { return last_move_; }

private:

    inline Piece::Piece256& operator[](int sq) { return square(sq); }

    inline Piece::Piece256& square(int sq)
    {
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }
    
    Piece::Piece256 square_[16 * 12];

    int side_ = -1;
    int flags_ = 0;
    int ep_sq_ = SquareNone;
    int half_moves_ = 0;
    int full_moves_ = 1;

    uint64_t key_ = 0;
    
    Gen::Move last_move_ = 0;

    Piece::List piece_list_[12];
    Piece::List pinned_;
};

#endif
