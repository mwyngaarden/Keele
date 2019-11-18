#ifndef POS_H
#define POS_H

#include <bitset>
#include <string>
#include <vector>
#include <cassert>
#include <cstdint>
#include "gen.h"
#include "hash.h"
#include "move.h"
#include "piece.h"
#include "square.h"
#include "types.h"
#include "util.h"

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

    int is_ok(bool incheck = true) const;

    void   make_move(const Move& move,       Undo& undo);
    void unmake_move(const Move& move, const Undo& undo);

    uint64_t      key() const { return key_; }
    uint64_t calc_key() const;

    void mark_pins(BitSet& pins) const;

    void add_piece(int sq, u8 p, bool update_key);
    void rem_piece(int sq, bool update_key);
    void mov_piece(int orig, int dest, bool update_key);

    bool ep_is_valid() const;

    bool move_is_legal(const Move& move) const;
    bool move_is_legal_ep(const Move& move) const;

    bool side_attacks(int side, int dest) const;
    bool piece_attacks(int orig, int dest) const;

    u8 at(int sq) const
    { 
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }

    inline const u8& operator[](int sq) const { return square(sq); }
    
    inline const u8& square(int sq) const
    {
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }

    inline bool is_empty(int sq) const
    {
        assert(sq88_is_ok(sq));

        return square(sq) == PieceNone256;
    }

    bool is_empty(int orig, int dest) const;

    inline bool is_op(int sq) const
    {
        assert(sq88_is_ok(sq));

        return square(sq) & (BlackFlag256 >> side_);
    }

    inline bool is_me(int sq) const
    {
        assert(sq88_is_ok(sq));

        return square(sq) & (WhiteFlag256 << side_);
    }

    inline bool is_piece(int sq, int piece) const
    {
        assert(sq88_is_ok(sq));
        assert(piece_is_ok(piece));

        return to_piece(square(sq)) == piece;
    }

    inline const PieceList& piece_list(int p12) const
    {
        assert(piece12_is_ok(p12));

        return piece_list_[p12];
    }

    inline int king_sq(int side) const
    {
        assert(side_is_ok(side));

        return piece_list_[WhiteKing12 + side][0];
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

    inline int checkers()      const { return checkers_count_; }
    inline int checkers(int i) const { return checkers_sq_[i]; }

private:

    void set_checkers_slow();
    void set_checkers_fast(const Move& move);

    inline u8& operator[](int sq) { return square(sq); }

    inline u8& square(int sq)
    {
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }
    
    u8 square_[16 * 12];

    int side_ = -1;
    int flags_ = 0;
    int ep_sq_ = SquareNone;
    int half_moves_ = 0;
    int full_moves_ = 1;

    int checkers_sq_[2];
    int checkers_count_ = 0;

    u64 key_ = 0;
    
    PieceList piece_list_[12];
};

#endif
