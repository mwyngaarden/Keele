#ifndef STRING_H
#define STRING_H

#include <string>
#include <vector>
#include <cstdint>

class Tokenizer {
public:
    Tokenizer(const std::string& s, char delim = ' ');

    std::size_t size() const;

    const std::string& operator[](std::size_t i) const;

private:
    std::vector<std::string> tokens_;
};


#endif
