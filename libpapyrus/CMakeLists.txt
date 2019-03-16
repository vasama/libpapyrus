add_library(libpapyrus
	src/Asm.c
	src/EliminateSSA.c
	src/Emit.c
	src/Eval.c
	src/DumpAsm.c
	src/DumpAST.c
	src/IR.c
	src/Parser.c
	src/Program.c
	src/Script.c
	src/SourceMap.c

	src/ulex/ulex.c
)
target_include_directories(libpapyrus PUBLIC include)
target_link_libraries(libpapyrus common)
set_c_standard(libpapyrus 11)
enable_warnings(libpapyrus)
