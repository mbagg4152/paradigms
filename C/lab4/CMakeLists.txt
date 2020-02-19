cmake_minimum_required(VERSION 3.10)

# set the project name
project(Lab4)

set(CMAKE_CXX_STANDARD 14)

# add the executable
add_executable(comp compare.c)
add_executable(lex lex.c)
add_executable(spell spellCheck.c)
add_executable(compn other-code/compare.c)
add_executable(lexn other-code/lex.c)
add_executable(spelln other-code/spellCheck.c)