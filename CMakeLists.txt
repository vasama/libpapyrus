cmake_minimum_required(VERSION 3.1)

project(libpapyrus)

function(set_c_standard target std)
	if (MSVC)
		target_compile_options(${target} PRIVATE -Xclang "-std=c${std}")
	else()
		target_compile_features(${target} PRIVATE "c_std_${std}")
		set_property(TARGET ${target} PROPERTY C_EXTENSIONS OFF)
	endif()
endfunction(set_c_standard)

function(enable_warnings target)
	target_compile_options(${target} PRIVATE -Werror=all -Werror=extra)

	if(NOT ${CMAKE_C_COMPILER_ID} STREQUAL GNU)
		target_compile_options(${target} PRIVATE
			-Werror=pedantic
			-Wshadow
			-Wno-gnu-zero-variadic-macro-arguments
		)
	endif()
endfunction(enable_warnings)

if(MSVC)
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
set_c_standard(libpapyrus 11)
enable_warnings(libpapyrus)

add_subdirectory(test)
