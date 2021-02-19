#ifndef TT_H
#define TT_H

#include "move.h"
#include "types.h"

// separate type for tt move?

constexpr const int DataCount = 16;
constexpr const int ClusterCountg = 4;

class TTE {
public:
	u32 key;

	u32 move;

	i8 depth;
	i8 depth_min;
	i8 depth_max;

	u8 date;

	u8 flags;

	i16 value_min;
	i16 value_max;
};

class TT {
public:
	TTE* tte = nullptr;

	u32 size = 0;
	u32 mask = 0;
	i32 date = -1;

	int age[DataCount];

	u32 used = 0;

	i64 read_count;
	u64 read_hit;

	u64 write_count;
	u64 write_hit;
	u64 write_coll;
};

#endif
