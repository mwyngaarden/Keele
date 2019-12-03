#!/bin/bash

rm -f *.gcda a.out

g++ -std=c++17 -DNDEBUG -march=native -Ofast -flto -fno-rtti -fno-exceptions -fprofile-generate *.cpp

./a.out 3

g++ -std=c++17 -DNDEBUG -march=native -Ofast -flto -fno-rtti -fno-exceptions -fprofile-use *.cpp
