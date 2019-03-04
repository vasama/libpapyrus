cmake_minimum_required(VERSION 3.1)

project(libpapyrus)

function(enable_warnings target)
	if (MSVC)
		target_compile_options(${target} PRIVATE /W4 /WX)
	else()
		target_compile_options(${target} PRIVATE -Wall -Wextra -Werror)
	endif()

	target_compile_options(${target} PRIVATE
		-Wpedantic
		-Wno-extra-semi
		-Wno-microsoft-enum-value
		-Wno-gnu-zero-variadic-macro-arguments
	)
endfunction(enable_warnings)

if(MSVC)
	SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Xclang -std=c11")
	add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

add_library(libpapyrus
	src/Arena.c
	src/Array.c
	src/Except.c
	src/FNV.c
	src/HashTable.c
	src/ObjectGraph.c
	src/String.c

	src/Eval.c
	src/DumpAST.c
	src/Parser.c
	src/Program.c
	src/Script.c
	src/SourceMap.c

	src/ulex/ulex.c
)
target_include_directories(libpapyrus PUBLIC include)
set_property(TARGET libpapyrus PROPERTY C_STANDARD 11)
set_property(TARGET libpapyrus PROPERTY C_EXTENSIONS OFF)
enable_warnings(libpapyrus)

add_subdirectory(test)
