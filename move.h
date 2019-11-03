#ifndef MOVE_H
#define MOVE_H

#include <cassert>
#include <cstdint>
#include "piece.h"
#include "square.h"
#include "util.h"

namespace Gen {
        
    class Move {
    public:
    
        using List = Util::List<Move, 256>;

        static constexpr uint32_t CaptureFlags         = 0xff << 16;

        static constexpr uint32_t PromoKnightFlag      = Piece::Knight << 24;
        static constexpr uint32_t PromoBishopFlag      = Piece::Bishop << 24;
        static constexpr uint32_t PromoRookFlag        = Piece::Rook   << 24;
        static constexpr uint32_t PromoQueenFlag       = Piece::Queen  << 24;

        static constexpr uint32_t PromoFlags           = 0x07 << 24;

        static constexpr uint32_t DoubleFlag           = 1 << 27;
        static constexpr uint32_t EPFlag               = 1 << 28;
        static constexpr uint32_t RevealCheckFlag      = 1 << 29;
        static constexpr uint32_t DirectCheckFlag      = 1 << 30;
        static constexpr uint32_t CastleFlag           = 1 << 31;
        
        static constexpr uint32_t CheckFlags           = DirectCheckFlag | RevealCheckFlag;
        static constexpr uint32_t EPDirectCheckFlags   = DirectCheckFlag | DoubleFlag;
        static constexpr uint32_t SpecialFlags         = EPFlag | CastleFlag | PromoFlags;

        inline Move() { }
        inline Move(uint32_t data) : data_(data) { }
        inline Move(int orig, int dest, Piece::Piece256 capture_piece = Piece::PieceNone256)
        {
            assert(is_sq88(orig));
            assert(is_sq88(dest));

            data_ = (capture_piece << 16) | (dest << 8) | orig;
        }

        inline operator uint32_t() const { return data_; }

        inline int             orig()          const { return (data_ >>  0) & 0xff; }
        inline int             dest()          const { return (data_ >>  8) & 0xff; }
        inline Piece::Piece256 capture_piece() const { return (data_ >> 16) & 0xff; }
        inline int             promo_piece()   const { return (data_ >> 24) & 0x07; }

        inline void set_dir_check()           { data_ |= DirectCheckFlag; }
        inline void set_rev_check()           { data_ |= RevealCheckFlag; }

        inline bool is_capture()        const { return data_ & CaptureFlags; }
        inline bool is_promo()          const { return data_ & PromoFlags; }

        inline bool is_castle()         const { return data_ & CastleFlag; }
        inline bool is_ep()             const { return data_ & EPFlag; }
        inline bool is_ep_or_castle()   const { return data_ & (EPFlag | CastleFlag); }
        inline bool is_double()         const { return data_ & DoubleFlag; }

        inline bool is_rev_check()      const { return data_ & RevealCheckFlag; }
        inline bool is_dir_check()      const { return data_ & DirectCheckFlag; }
        inline bool is_double_check()   const { return (data_ & CheckFlags) == CheckFlags; }
        inline bool is_check()          const { return data_ & CheckFlags; }
        inline bool is_ep_dir_check()   const { return (data_ & EPDirectCheckFlags) == EPDirectCheckFlags; }

        inline bool is_special()        const { return data_ & SpecialFlags; }

    private:
        uint32_t data_;
    };

    struct Undo {
        int flags;
        int ep_sq;
        int half_moves;
        int full_moves;

        int check_sq[2];

        Gen::Move last_move;
    };
}

#endif
