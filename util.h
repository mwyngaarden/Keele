#ifndef UTIL_H
#define UTIL_H

#include <cassert>
#include <cstdint>
#include <iostream>

#define KEELE_VERSION "1.0"
#define KEELE_AUTHOR "Martin Wyngaarden"

namespace Util {

    template <typename T, std::size_t N>
    class List {
    public:
        static constexpr std::size_t npos = -1;
        static constexpr std::size_t capacity() { return N; }
        static constexpr std::size_t max_size() { return N; }

        std::size_t size() const { return size_; }
        bool empty() const { return size_ == 0; }

        void clear() { size_ = 0; }

        void add(T& value)
        {
            assert(size_ < N);

            data_[size_++] = value;
        }

        void add(T&& value)
        {
            assert(size_ < N);

            data_[size_++] = std::move(value);
        }

        std::size_t find(T value) const
        {
            for (std::size_t i = 0; i < size_; i++)
                if (data_[i] == value)
                    return i;

            return npos;
        }

        void remove(T value)
        {
            assert(find(value) != npos);

            T* p = data_;

            while (*p != value) ++p;

            *p = data_[--size_];
        }
            
        void replace(T old_value, T new_value)
        {
            assert(find(old_value) != npos);
            assert(find(new_value) == npos);

            T* p = data_;

            while (*p != old_value) ++p;

            *p = new_value;
        }

        T* begin() { return data_; } 
        T* end() { return data_ + size_; }

        const T* begin() const { return data_; } 
        const T* end() const { return data_ + size_; }

        T& operator[](std::size_t i)
        {
            assert(i < size_);

            return data_[i];
        }
        
        const T& operator[](std::size_t i) const
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
