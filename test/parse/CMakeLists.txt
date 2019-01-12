add_executable(parse
	src/Main.c
)
target_link_libraries(parse libpapyrus)
set_property(TARGET libpapyrus PROPERTY C_STANDARD 99)
