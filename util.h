#ifndef UTIL_H
#define UTIL_H

#include <cassert>
#include <cstdint>

namespace Util {

    template <typename T, std::size_t N>
    class List {
    public:
        static constexpr std::size_t npos = -1;
        static constexpr std::size_t capacity() { return N; }

        std::size_t size() const { return size_; }

        inline void add(T value)
        {
            assert(size_ < N);

            data_[size_++] = value;
        }

        inline std::size_t find(T value) const
        {
            for (std::size_t i = 0; i < size_; i++)
                if (data_[i] == value)
                    return i;

            return npos;
        }

        inline void remove(T value)
        {
            assert(find(value) != npos);

            T* p = data_;

            while (*p != value) ++p;

            *p = data_[--size_];
        }
            
        inline void replace(T old_value, T new_value)
        {
            assert(find(old_value) != npos);
            assert(find(new_value) == npos);

            T* p = data_;

            while (*p != old_value) ++p;

            *p = new_value;
        }

        inline const T* begin() const { return data_; } 
        inline const T* end() const { return data_ + size_; }

        inline T operator[](std::size_t i) const
        {
            assert(i < size_);

            return data_[i];
        }

    private:
        T data_[N];
        std::size_t size_ = 0;
    };
}








#endif
