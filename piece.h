#ifndef PIECE_H
#define PIECE_H

#include <cstdint>
#include "util.h"

namespace Piece {

    using List = Util::List<int, 10>;
    
    void init();

    enum {
        White,
        Black
    };

    enum {
        Pawn,
        Knight,
        Bishop,
        Rook,
        Queen,
        King
    };

    enum {
        WhitePawn12,
        BlackPawn12,
        WhiteKnight12,
        BlackKnight12,
        WhiteBishop12,
        BlackBishop12,
        WhiteRook12,
        BlackRook12,
        WhiteQueen12,
        BlackQueen12,
        WhiteKing12,
        BlackKing12,
        PieceNone12
    };

    typedef uint8_t Piece256;

    constexpr Piece256 PieceNone256     = 0;

    constexpr Piece256 WhiteFlag256     = 1 << 0;
    constexpr Piece256 BlackFlag256     = 1 << 1;
    constexpr Piece256 ColorFlags256    = WhiteFlag256 | BlackFlag256;

    constexpr Piece256 WhitePawnFlag256 = 1 << 2;
    constexpr Piece256 BlackPawnFlag256 = 1 << 3;
    constexpr Piece256 PawnFlags256     = WhitePawnFlag256 | BlackPawnFlag256;

    constexpr Piece256 KnightFlag256    = 1 << 4;
    constexpr Piece256 BishopFlag256    = 1 << 5;
    constexpr Piece256 RookFlag256      = 1 << 6;
    constexpr Piece256 QueenFlags256    = BishopFlag256 | RookFlag256;
    constexpr Piece256 KingFlag256      = 1 << 7;
    constexpr Piece256 PieceFlags256    = PawnFlags256 | KnightFlag256 | QueenFlags256 | KingFlag256;

    constexpr Piece256 WhitePawn256     = WhiteFlag256 | WhitePawnFlag256;
    constexpr Piece256 WhiteKnight256   = WhiteFlag256 | KnightFlag256;
    constexpr Piece256 WhiteBishop256   = WhiteFlag256 | BishopFlag256;
    constexpr Piece256 WhiteRook256     = WhiteFlag256 | RookFlag256;
    constexpr Piece256 WhiteQueen256    = WhiteFlag256 | QueenFlags256;
    constexpr Piece256 WhiteKing256     = WhiteFlag256 | KingFlag256;

    constexpr Piece256 BlackPawn256     = BlackFlag256 | BlackPawnFlag256;
    constexpr Piece256 BlackKnight256   = BlackFlag256 | KnightFlag256;
    constexpr Piece256 BlackBishop256   = BlackFlag256 | BishopFlag256;
    constexpr Piece256 BlackRook256     = BlackFlag256 | RookFlag256;
    constexpr Piece256 BlackQueen256    = BlackFlag256 | QueenFlags256;
    constexpr Piece256 BlackKing256     = BlackFlag256 | KingFlag256;

    constexpr Piece256 PieceInvalid256  = ~(ColorFlags256 | PawnFlags256);

    // methods

    constexpr bool is_white         (Piece256 piece) { return (piece & WhiteFlag256) != PieceNone256; }
    constexpr bool is_black         (Piece256 piece) { return (piece & BlackFlag256) != PieceNone256; }

    constexpr bool is_white_pawn    (Piece256 piece) { return piece == WhitePawn256; }
    constexpr bool is_black_pawn    (Piece256 piece) { return piece == BlackPawn256; }

    constexpr bool is_pawn          (Piece256 piece) { return (piece & PawnFlags256) != PieceNone256; }
    constexpr bool is_knight        (Piece256 piece) { return (piece & KnightFlag256) != PieceNone256; }
    constexpr bool is_bishop        (Piece256 piece) { return (piece & QueenFlags256) == BishopFlag256; }
    constexpr bool is_rook          (Piece256 piece) { return (piece & QueenFlags256) == RookFlag256; }
    constexpr bool is_queen         (Piece256 piece) { return (piece & QueenFlags256) == QueenFlags256; }
    constexpr bool is_king          (Piece256 piece) { return (piece & KingFlag256) != PieceNone256; }

    constexpr bool is_slider        (Piece256 piece) { return (piece & QueenFlags256) != PieceNone256; }
    
    constexpr int flip_side         (int side) { return side ^ 1; }

    char to_char                    (int piece);
    char to_char                    (Piece256 piece);
    int to_piece                    (Piece256 piece);
    int to_piece12                  (Piece256 piece);
    int to_piece12                  (int side, int piece);
    Piece256 to_piece256            (int side, int piece);
    Piece256 to_piece256            (char c);

    constexpr bool side_is_ok       (int side) { return side == 0 || side == 1; }
    constexpr bool piece_is_ok      (int piece) { return piece >= Pawn && piece <= King; }
    constexpr bool piece12_is_ok    (int piece) { return piece >= WhitePawn12 && piece <= BlackKing12; }

    bool piece256_is_ok   (Piece256 piece);
}

#endif
