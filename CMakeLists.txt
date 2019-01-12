cmake_minimum_required(VERSION 3.1)

project(libpapyrus)

if(MSVC)
	add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

add_library(libpapyrus
	src/Papyrus.c
	src/ulex.c
	src/DumpAST.c
)
target_include_directories(libpapyrus PUBLIC include)
set_property(TARGET libpapyrus PROPERTY C_STANDARD 99)

add_subdirectory(test)
