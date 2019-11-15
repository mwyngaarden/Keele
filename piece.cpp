#include <cassert>
#include <cstddef>
#include <cstdint>
#include "piece.h"
#include "types.h"
using namespace std;

static int piece256_to_piece12[256];

constexpr char piece12_to_char[12] = { 'P', 'p', 'N', 'n', 'B', 'b', 'R', 'r', 'Q', 'q', 'K', 'k' };

constexpr u8 piece12_to_piece256[12] = {
    WhitePawn256,
    BlackPawn256,
    WhiteKnight256,
    BlackKnight256,
    WhiteBishop256,
    BlackBishop256,
    WhiteRook256,
    BlackRook256,
    WhiteQueen256,
    BlackQueen256,
    WhiteKing256,
    BlackKing256
};

void piece_init()
{
    for (int i = 0; i < 256; i++)
        piece256_to_piece12[i] = PieceNone12;

    piece256_to_piece12[WhitePawn256]   = WhitePawn12;
    piece256_to_piece12[BlackPawn256]   = BlackPawn12;
    piece256_to_piece12[WhiteKnight256] = WhiteKnight12;
    piece256_to_piece12[BlackKnight256] = BlackKnight12;
    piece256_to_piece12[WhiteBishop256] = WhiteBishop12;
    piece256_to_piece12[BlackBishop256] = BlackBishop12;
    piece256_to_piece12[WhiteRook256]   = WhiteRook12;
    piece256_to_piece12[BlackRook256]   = BlackRook12;
    piece256_to_piece12[WhiteQueen256]  = WhiteQueen12;
    piece256_to_piece12[BlackQueen256]  = BlackQueen12;
    piece256_to_piece12[WhiteKing256]   = WhiteKing12;
    piece256_to_piece12[BlackKing256]   = BlackKing12;
}

bool piece256_is_ok(u8 piece)
{
    return piece256_to_piece12[piece] != PieceNone12;
}

int to_piece12(u8 piece)
{
    assert(piece256_is_ok(piece));

    return piece256_to_piece12[piece];
}

int to_piece(u8 piece)
{
    assert(piece256_is_ok(piece));

    return to_piece12(piece) >> 1;
}

int to_piece12(int side, int piece)
{
    assert(side_is_ok(side));
    assert(piece_is_ok(piece));

    return (piece << 1) | side;
}

u8 to_piece256(int side, int piece)
{
    assert(side_is_ok(side));
    assert(piece_is_ok(piece));

    return piece12_to_piece256[to_piece12(side, piece)];
}

u8 char_to_piece256(char c)
{
    switch (c) {
    case 'P': return WhitePawn256;
    case 'N': return WhiteKnight256;
    case 'B': return WhiteBishop256;
    case 'R': return WhiteRook256;
    case 'Q': return WhiteQueen256;
    case 'K': return WhiteKing256;
    case 'p': return BlackPawn256;
    case 'n': return BlackKnight256;
    case 'b': return BlackBishop256;
    case 'r': return BlackRook256;
    case 'q': return BlackQueen256;
    case 'k': return BlackKing256;
    default:
        assert(false);
        return PieceInvalid256;
    }
}

char piece256_to_char(u8 piece)
{
    assert(piece256_is_ok(piece));

    return piece12_to_char[piece256_to_piece12[piece]];
}
