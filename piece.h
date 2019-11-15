#ifndef PIECE_H
#define PIECE_H

#include <cstdint>
#include "types.h"
#include "util.h"

using PieceList = Util::List<int, 10>;

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

constexpr int PieceList12[2][6] = {
    { WhitePawn12, WhiteKnight12, WhiteBishop12, WhiteRook12, WhiteQueen12, WhiteKing12 },
    { BlackPawn12, BlackKnight12, BlackBishop12, BlackRook12, BlackQueen12, BlackKing12 }
};

constexpr u8 PieceNone256     = 0;

constexpr u8 WhiteFlag256     = 1 << 0;
constexpr u8 BlackFlag256     = 1 << 1;
constexpr u8 ColorFlags256    = WhiteFlag256 | BlackFlag256;

constexpr u8 WhitePawnFlag256 = 1 << 2;
constexpr u8 BlackPawnFlag256 = 1 << 3;
constexpr u8 PawnFlags256     = WhitePawnFlag256 | BlackPawnFlag256;

constexpr u8 KnightFlag256    = 1 << 4;
constexpr u8 BishopFlag256    = 1 << 5;
constexpr u8 RookFlag256      = 1 << 6;
constexpr u8 QueenFlags256    = BishopFlag256 | RookFlag256;
constexpr u8 KingFlag256      = 1 << 7;
constexpr u8 PieceFlags256    = PawnFlags256 | KnightFlag256 | QueenFlags256 | KingFlag256;

constexpr u8 WhitePawn256     = WhiteFlag256 | WhitePawnFlag256;
constexpr u8 WhiteKnight256   = WhiteFlag256 | KnightFlag256;
constexpr u8 WhiteBishop256   = WhiteFlag256 | BishopFlag256;
constexpr u8 WhiteRook256     = WhiteFlag256 | RookFlag256;
constexpr u8 WhiteQueen256    = WhiteFlag256 | QueenFlags256;
constexpr u8 WhiteKing256     = WhiteFlag256 | KingFlag256;

constexpr u8 BlackPawn256     = BlackFlag256 | BlackPawnFlag256;
constexpr u8 BlackKnight256   = BlackFlag256 | KnightFlag256;
constexpr u8 BlackBishop256   = BlackFlag256 | BishopFlag256;
constexpr u8 BlackRook256     = BlackFlag256 | RookFlag256;
constexpr u8 BlackQueen256    = BlackFlag256 | QueenFlags256;
constexpr u8 BlackKing256     = BlackFlag256 | KingFlag256;

constexpr u8 PieceInvalid256  = ~(ColorFlags256 | PawnFlags256);

// methods

void piece_init();

constexpr bool is_white     (u8 piece) { return (piece & WhiteFlag256) != PieceNone256; }
constexpr bool is_black     (u8 piece) { return (piece & BlackFlag256) != PieceNone256; }

constexpr bool is_pawn      (u8 piece) { return (piece & PawnFlags256) != PieceNone256; }
constexpr bool is_knight    (u8 piece) { return (piece & KnightFlag256) != PieceNone256; }
constexpr bool is_bishop    (u8 piece) { return (piece & QueenFlags256) == BishopFlag256; }
constexpr bool is_rook      (u8 piece) { return (piece & QueenFlags256) == RookFlag256; }
constexpr bool is_queen     (u8 piece) { return (piece & QueenFlags256) == QueenFlags256; }
constexpr bool is_king      (u8 piece) { return (piece & KingFlag256) != PieceNone256; }
constexpr bool is_slider    (u8 piece) { return (piece & QueenFlags256) != PieceNone256; }

constexpr u8   flip_pawn    (u8 piece) { return piece ^ (ColorFlags256 | PawnFlags256); }
constexpr u8   make_pawn    (int side) { return WhitePawn256 << side; }
constexpr u8   make_flag    (int side) { return side + 1; }
constexpr int  flip_side    (int side) { return side ^ 1; }

constexpr bool side_is_ok   (int side) { return side == 0 || side == 1; }
constexpr bool piece_is_ok  (int piece){ return piece >= Pawn && piece <= King; }
constexpr bool piece12_is_ok(int piece){ return piece >= WhitePawn12 && piece <= BlackKing12; }

bool piece256_is_ok         (u8 piece);

char piece256_to_char       (u8 piece);
int  to_piece               (u8 piece);
int  to_piece12             (u8 piece);
u8   char_to_piece256       (char c);
u8   to_piece256            (int side, int piece);
int  to_piece12             (int side, int piece);

#endif
