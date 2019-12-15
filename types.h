#ifndef TYPES_H
#define TYPES_H

#include <bitset>
#include <cassert>
#include <cstdint>


constexpr bool GenerateLegal = true;

constexpr bool UpdateInfo = true;


#ifndef NDEBUG
constexpr bool Debug = true;
#else
constexpr bool Debug = false;
#endif


using BitSet = std::bitset<128>;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;


constexpr int PliesMax = 128;
constexpr int MovesMax = 256;

// TODO: types for Score and Value


// eval

constexpr int ValueDraw =     0;
constexpr int ValueMate = 32000;
constexpr int ValueInf  = 32001;
constexpr int ValueNone = 32002;

constexpr int ValueMateInPliesMax  =  ValueMate - PliesMax;
constexpr int ValueMatedInPliesMax = -ValueMate + PliesMax;

constexpr int ValuePawn   =  100;
constexpr int ValueKnight =  400;
constexpr int ValueBishop =  425;
constexpr int ValueRook   =  650;
constexpr int ValueQueen  = 1250;

// tt

constexpr int BoundNone  = 0;
constexpr int BoundUpper = 1;
constexpr int BoundLower = 2;
constexpr int BoundExact = BoundUpper | BoundLower;

#endif
