#ifndef LIST_H
#define LIST_H

#include <cstdint>

template <typename T, std::size_t N>
class List {
public:

    constexpr std::size_t capacity() const { return N; }

    std::size_t size() const { return size_; }


    inline void add(T data)
    {
        assert(size_ < N);

        data_[size_++] = data;
    }

    inline std::size_t find(T data) const
    {
        for (std::size_t i = 0; i < size_; i++)
            if (data_[i] == data)
                return i;

        return N;
    }

    inline void rem(T data)
    {
        assert(find(data) != N);

        T* p = data_;

        while (*p != data) ++p;

        *p = data_[--size_];
    }
        
    inline void replace(T a, T b)
    {
        assert(find(a) != N);
        assert(find(b) == N);

        T* p = data_;

        while (*p != a) ++p;

        *p = b;
    }

    inline const T* begin() const { return data_; } 
    inline const T* end  () const { return data_ + size_; }

    inline T operator[](std::size_t i) const
    {
        assert(i < size_);

        return data_[i];
    }

private:
    T data_[N];

    std::size_t size_ = 0;
};

#endif
