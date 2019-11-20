#ifndef MOVE_H
#define MOVE_H

#include <sstream>
#include <string>
#include <cassert>
#include <cstdint>
#include "piece.h"
#include "square.h"
#include "types.h"
#include "util.h"
    
class Move {
public:
    static constexpr u32 CaptureFlags       = 0xff << 14;

    static constexpr u32 PromoKnightFlag    = Knight << 22;
    static constexpr u32 PromoBishopFlag    = Bishop << 22;
    static constexpr u32 PromoRookFlag      = Rook   << 22;
    static constexpr u32 PromoQueenFlag     = Queen  << 22;

    static constexpr u32 PromoFlags         = PromoQueenFlag | PromoRookFlag
                                            | PromoBishopFlag | PromoKnightFlag;

    static constexpr u32 DoubleFlag         = 1 << 25;
    static constexpr u32 CastleFlag         = 1 << 26;
    static constexpr u32 EPFlag             = 1 << 27;
    static constexpr u32 DirectCheckFlag    = 1 << 28;
    static constexpr u32 RevealCheckFlag    = 1 << 29;
    static constexpr u32 EPRevRevCheckFlag  = 1 << 30;
    
    static constexpr u32 CheckFlags         = DirectCheckFlag | RevealCheckFlag | EPRevRevCheckFlag;

    
    Move() { }
    Move(u32 data) : data_(data) { }
    Move(int orig, int dest, u8 capture_piece = PieceNone256)
    {
        assert(sq88_is_ok(orig));
        assert(sq88_is_ok(dest));
        assert(!is_king(capture_piece));

        data_ = (capture_piece << 14) | (dest << 7) | orig;
    }

    void set_dir_check()           { data_ |= DirectCheckFlag; }
    void set_rev_check()           { data_ |= RevealCheckFlag; }
    void set_rev_rev_check()       { data_ |= EPRevRevCheckFlag; }

    operator u32() const { return data_; }

    int  orig()              const { return (data_ >>  0) & 0x7f; }
    int  dest()              const { return (data_ >>  7) & 0x7f; }
    u8   capture_piece()     const { return (data_ >> 14) & 0xff; }
    int  promo_piece()       const { return (data_ >> 22) & 0x07; }

    bool is_capture()        const { return data_ & CaptureFlags; }
    bool is_promo()          const { return data_ & PromoFlags; }
    bool is_castle()         const { return data_ & CastleFlag; }
    bool is_ep()             const { return data_ & EPFlag; }
    bool is_ep_or_castle()   const { return data_ & (EPFlag | CastleFlag); }
    bool is_double()         const { return data_ & DoubleFlag; }

    bool is_special()        const { return data_ & (EPFlag | PromoFlags | CastleFlag); }

    bool is_check()          const { return data_ & CheckFlags; }
    bool is_double_check()   const { return is_dir_rev_check() || is_rev_rev_check(); }

    bool is_dir_check()      const { return (data_ & CheckFlags) == DirectCheckFlag; }
    bool is_rev_check()      const { return (data_ & CheckFlags) == RevealCheckFlag; }
    bool is_dir_rev_check()  const { return (data_ & CheckFlags) == (DirectCheckFlag | RevealCheckFlag); }
    bool is_rev_rev_check()  const { return (data_ & CheckFlags) == (RevealCheckFlag | EPRevRevCheckFlag); }

private:
    u32 data_;
};

struct Undo {
    int flags;
    int ep_sq;
    int half_moves;
    int full_moves;

    u64 key;

    int checkers_sq[2];
    int checkers_count;
};
    
using MoveList = Util::List<Move, 256>;

#endif
