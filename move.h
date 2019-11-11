#ifndef MOVE_H
#define MOVE_H

#include <sstream>
#include <string>
#include <cassert>
#include <cstdint>
#include "piece.h"
#include "square.h"
#include "util.h"

namespace Gen {
        
    class Move {
    public:
    
        using List = Util::List<Move, 256>;

        static constexpr uint32_t CaptureFlags         = 0xff << 14;

        static constexpr uint32_t PromoKnightFlag      = Piece::Knight << 22;
        static constexpr uint32_t PromoBishopFlag      = Piece::Bishop << 22;
        static constexpr uint32_t PromoRookFlag        = Piece::Rook   << 22;
        static constexpr uint32_t PromoQueenFlag       = Piece::Queen  << 22;

        static constexpr uint32_t PromoFlags           = PromoQueenFlag | PromoRookFlag
                                                       | PromoBishopFlag | PromoKnightFlag;

        static constexpr uint32_t DoubleFlag           = 1 << 25;
        static constexpr uint32_t CastleFlag           = 1 << 26;
        static constexpr uint32_t EPFlag               = 1 << 27;
        static constexpr uint32_t DirectCheckFlag      = 1 << 28;
        static constexpr uint32_t RevealCheckFlag      = 1 << 29;
        static constexpr uint32_t EPRevRevCheckFlag    = 1 << 30;
        
        static constexpr uint32_t CheckFlags           = DirectCheckFlag | RevealCheckFlag | EPRevRevCheckFlag;

        inline bool is_dir_check()      const { return (data_ & CheckFlags) == DirectCheckFlag; }
        inline bool is_rev_check()      const { return (data_ & CheckFlags) == RevealCheckFlag; }
        inline bool is_dir_rev_check()  const { return (data_ & CheckFlags) == (DirectCheckFlag | RevealCheckFlag); }
        inline bool is_rev_rev_check()  const { return (data_ & CheckFlags) == (RevealCheckFlag | EPRevRevCheckFlag); }
        
        inline Move() { }
        inline Move(uint32_t data) : data_(data) { }
        inline Move(int orig, int dest, Piece::Piece256 capture_piece = Piece::PieceNone256)
        {
            assert(is_sq88(orig));
            assert(is_sq88(dest));

            data_ = (capture_piece << 14) | (dest << 7) | orig;
        }

        inline operator uint32_t() const { return data_; }

        inline int             orig()          const { return (data_ >>  0) & 0x7f; }
        inline int             dest()          const { return (data_ >>  7) & 0x7f; }
        inline Piece::Piece256 capture_piece() const { return (data_ >> 14) & 0xff; }
        inline int             promo_piece()   const { return (data_ >> 22) & 0x07; }

        inline void set_dir_check()           { data_ |= DirectCheckFlag; }
        inline void set_rev_check()           { data_ |= RevealCheckFlag; }
        inline void set_rev_rev_check()       { data_ |= EPRevRevCheckFlag; }

        inline bool is_capture()        const { return data_ & CaptureFlags; }
        inline bool is_promo()          const { return data_ & PromoFlags; }

        inline bool is_castle()         const { return data_ & CastleFlag; }
        inline bool is_ep()             const { return data_ & EPFlag; }
        inline bool is_ep_or_castle()   const { return data_ & (EPFlag | CastleFlag); }
        inline bool is_double()         const { return data_ & DoubleFlag; }

        inline bool is_check()          const { return data_ & CheckFlags; }
        inline bool is_special()        const { return data_ & (EPFlag | PromoFlags | CastleFlag); }

    private:
        uint32_t data_;
    };

    struct Undo {
        int flags;
        int ep_sq;
        int half_moves;
        int full_moves;

        uint64_t key;

        int check_sq[2];

        Gen::Move last_move;
    };
}

#endif
