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

    
    inline Move() { }
    inline Move(u32 data) : data_(data) { }
    inline Move(int orig, int dest, u8 capture_piece = PieceNone256)
    {
        assert(sq88_is_ok(orig));
        assert(sq88_is_ok(dest));
        assert(!is_king(capture_piece));

        data_ = (capture_piece << 14) | (dest << 7) | orig;
    }

    inline void set_dir_check()           { data_ |= DirectCheckFlag; }
    inline void set_rev_check()           { data_ |= RevealCheckFlag; }
    inline void set_rev_rev_check()       { data_ |= EPRevRevCheckFlag; }

    inline operator u32() const { return data_; }

    inline int  orig()              const { return (data_ >>  0) & 0x7f; }
    inline int  dest()              const { return (data_ >>  7) & 0x7f; }
    inline u8   capture_piece()     const { return (data_ >> 14) & 0xff; }
    inline int  promo_piece()       const { return (data_ >> 22) & 0x07; }

    inline bool is_capture()        const { return data_ & CaptureFlags; }
    inline bool is_promo()          const { return data_ & PromoFlags; }
    inline bool is_castle()         const { return data_ & CastleFlag; }
    inline bool is_ep()             const { return data_ & EPFlag; }
    inline bool is_ep_or_castle()   const { return data_ & (EPFlag | CastleFlag); }
    inline bool is_double()         const { return data_ & DoubleFlag; }

    inline bool is_special()        const { return data_ & (EPFlag | PromoFlags | CastleFlag); }

    inline bool is_check()          const { return data_ & CheckFlags; }
    inline bool is_double_check()   const { return is_dir_rev_check() || is_rev_rev_check(); }

    inline bool is_dir_check()      const { return (data_ & CheckFlags) == DirectCheckFlag; }
    inline bool is_rev_check()      const { return (data_ & CheckFlags) == RevealCheckFlag; }
    inline bool is_dir_rev_check()  const { return (data_ & CheckFlags) == (DirectCheckFlag | RevealCheckFlag); }
    inline bool is_rev_rev_check()  const { return (data_ & CheckFlags) == (RevealCheckFlag | EPRevRevCheckFlag); }

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
