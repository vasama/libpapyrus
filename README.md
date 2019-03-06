[![Build Status](https://travis-ci.com/vasama/libpapyrus.svg?branch=master)](https://travis-ci.com/vasama/libpapyrus)

# libpapyrus
libpapyrus is an analysis and compilation library for the Papyrus language used in Bethesda Game Studios' The Elder Scrolls V: Skyrim and Fallout 4 titles. The library aims to provide a complete optimizing compiler pipeline for the language.

The primary motivation for the development of libpapyrus was the need for an embeddable compiler library for the [OpenCK](https://github.com/Open-CK) project. In addition libpapyrus seeks to provide faster and safer code than existing compilers, particularly the official compiler shipping with the Creation Kit.

## Features
libpapyrus provides an exposed compiler pipeline separated into three distinct phases: syntactic analysis, semantic analysis and code generation. The syntactic analysis API allows parsing of source text, producing error information and hierarchical syntax descriptions. The semantic analysis API takes syntax trees produced in the previous phase, considering them as a group for the purposes of external symbol resolution across scripts. Type checking and other static analyses are performed, producing errors and warnings. Finally, the codegen API can be used to produce Papyrus assembly code and PEX form binaries from type-checked scripts.
