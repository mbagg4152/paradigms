cmake_minimum_required(VERSION 3.13)

# set the project name
project(Lab4)

set(CMAKE_CXX_STANDARD 14)

# add the executable
add_executable(comp compare.c)
add_executable(lex lex.c)
add_executable(spell spellCheck.c)