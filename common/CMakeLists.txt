add_library(common
	src/Arena.c
	src/Array.c
	src/Except.c
	src/FNV.c
	src/HashTable.c
	src/ObjectGraph.c
	src/String.c
)
target_include_directories(common PUBLIC include)
target_include_directories(common PUBLIC ../libpapyrus/include)
set_c_standard(common 11)
enable_warnings(common)
