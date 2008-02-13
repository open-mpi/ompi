AUTOMAKE_OPTIONS = -Wno-gnu

bin_PROGRAMS = opari

include_HEADERS = \
	../lib/opari_omp.h \
	../lib/pomp_lib.h

CXX = $(CXX_FOR_BUILD)
CXXFLAGS = $(CXXFLAGS_FOR_BUILD)
CXXCPP = $(CXXCPP_FOR_BUILD)
CPPFLAGS = $(CPPFLAGS_FOR_BUILD)
LDFLAGS = $(LDFLAGS_FOR_BUILD)
LIBS = $(LIBS_FOR_BUILD)

opari_SOURCES = \
	handler.h \
	ompragma.h \
	ompregion.h \
	opari.h \
	handler.cc \
	ompragma.cc \
	ompragma_c.cc \
	ompragma_f.cc \
	ompregion.cc \
	opari.cc \
	process_c.cc \
	process_f.cc \
	process_omp.cc

EXTRA_DIST = \
	fixinc/cctype \
	fixinc/cstddef \
	fixinc/cstdio \
	fixinc/cstdlib \
	fixinc/cstring

