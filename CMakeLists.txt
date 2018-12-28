cmake_minimum_required(VERSION 3.1)

project(libpapyrus)

add_library(libpapyrus
	src/Papyrus.c
	src/ulex.c
)
target_include_directories(libpapyrus PUBLIC include)
set_property(TARGET libpapyrus PROPERTY C_STANDARD 99)
