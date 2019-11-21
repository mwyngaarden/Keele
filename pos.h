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
    
    void   make_move(const Move& move,       Undo& undo);
    void unmake_move(const Move& move, const Undo& undo);

    std::string to_fen() const;

    int is_ok(bool check_test = true) const;

    uint64_t      key() const { return key_; }
    uint64_t calc_key() const;

    void mark_pins(BitSet& pins) const;

    bool ep_is_valid(int sq) const;

    bool move_is_legal(const Move& move) const;
    bool move_is_legal_ep(const Move& move) const;

    bool move_is_irreversible(const Move& move) const;

    bool side_attacks(int side, int dest) const;
    bool piece_attacks(int orig, int dest) const;

    u8 at(int sq) const
    { 
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }

    const u8& operator[](int sq) const
    {
        assert(sq >= -36 && sq < 156);
        
        return square(sq);
    }
    
    const u8& square(int sq) const
    {
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }

    bool empty(int sq) const
    {
        assert(sq88_is_ok(sq));

        return square(sq) == PieceNone256;
    }

    bool empty(int orig, int dest) const;

    bool is_op(int sq) const
    {
        assert(sq88_is_ok(sq));

        return square(sq) & (BlackFlag256 >> side_);
    }

    bool is_me(int sq) const
    {
        assert(sq88_is_ok(sq));

        return square(sq) & (WhiteFlag256 << side_);
    }

    bool is_piece(int sq, int piece) const
    {
        assert(sq88_is_ok(sq));
        assert(piece_is_ok(piece));

        return to_piece(square(sq)) == piece;
    }

    const PieceList& piece_list(int p12) const
    {
        assert(piece12_is_ok(p12));

        return piece_list_[p12];
    }

    int king_sq(int side) const
    {
        assert(side_is_ok(side));

        return piece_list_[WhiteKing12 + side][0];
    }

    int king_sq() const { return king_sq(side_); }

    int side()                  const { return side_; }
    int ep_sq()                 const { return ep_sq_; }
    int half_moves()            const { return half_moves_; }
    int full_moves()            const { return full_moves_; }

    bool can_castle_k(int side) const { return (flags_ & (WhiteCastleKFlag << side)) != 0; }
    bool can_castle_q(int side) const { return (flags_ & (WhiteCastleQFlag << side)) != 0; }
    bool can_castle  (int side) const { return (flags_ & (WhiteCastleFlags << side)) != 0; }
    bool can_castle_k()         const { return can_castle_k(side_); }
    bool can_castle_q()         const { return can_castle_q(side_); }
    bool can_castle  ()         const { return can_castle(side_); }

    // misc
    
    std::string dump() const;

    int checkers()      const { return checkers_count_; }
    int checkers(int i) const { return checkers_sq_[i]; }

    u8 pawn_file(int side, int file) const
    {
        assert(side_is_ok(side));
        assert(file_is_ok(file));

        return pawn_file_[side][1 + file];
    }

private:
    void add_piece(int sq, u8 piece256, bool update = false);
    void rem_piece(int sq, bool update = false);
    void mov_piece(int orig, int dest, bool update = false);

    void set_checkers_slow();
    void set_checkers_fast(const Move& move);

    u8& operator[](int sq) { return square(sq); }

    u8& square(int sq)
    {
        assert(sq >= -36 && sq < 156);

        return square_[36 + sq];
    }
    
    u8 square_[16 * 12];
    u8 pawn_file_[2][10];

    int side_ = -1;
    int flags_ = 0;
    int ep_sq_ = SquareNone;
    int half_moves_ = 0;
    int full_moves_ = 1;

    int checkers_count_ = 0;
    int checkers_sq_[2];

    u64 key_ = 0;

    PieceList piece_list_[12];
};

#endif
