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

std::string Trim        (std::string s);
std::string TrimBegin   (std::string s);
std::string TrimEnd     (std::string s);
std::string Sanitize    (std::string s);

#endif
