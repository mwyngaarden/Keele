#include <sstream>
#include <string>
#include <vector>

#include <cstring>

#include "string.h"

using namespace std;

Tokenizer::Tokenizer(const string& s, char delim)
{
    stringstream ss(s);
    string token;

    while (getline(ss, token, delim))
        tokens_.push_back(token);
}

size_t Tokenizer::size() const
{
    return tokens_.size();
}

const string& Tokenizer::operator[](size_t i) const
{
    return tokens_[i];
}

string TrimBegin(string s)
{
    while (!s.empty() && isspace(s[0]))
        s.erase(0, 1);

    return s;
}

string TrimEnd(string s)
{
    while (!s.empty() && isspace(s[s.size() - 1]))
        s.erase(s.size() - 1, 1);

    return s;
}

string Trim(string s)
{
    return TrimEnd(TrimBegin(s));
}


