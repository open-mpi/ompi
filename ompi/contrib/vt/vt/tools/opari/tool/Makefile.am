AUTOMAKE_OPTIONS = -Wno-gnu

if AMBUILDBINARIES
bin_PROGRAMS = opari
endif

include_HEADERS = \
	../lib/opari_omp.h \
	../lib/pomp_lib.h

AM_CPPFLAGS = -I$(top_srcdir)/util -I$(top_builddir)/util

CC = $(CC_FOR_BUILD)
CFLAGS = $(CFLAGS_FOR_BUILD)
CPP = $(CPP_FOR_BUILD)
CXX = $(CXX_FOR_BUILD)
CXXFLAGS = $(CXXFLAGS_FOR_BUILD)
CXXCPP = $(CXXCPP_FOR_BUILD)
CPPFLAGS = $(CPPFLAGS_FOR_BUILD)
LDFLAGS = $(LDFLAGS_FOR_BUILD)
LIBS = $(LIBS_FOR_BUILD)

# From Automake v2.0 the 'subdir-objects' option will be required to compile
# sources located outside of the current build directory. Due to a bug in
# Automake (see http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13928) we cannot
# enable this option, so we have to sym link in the following source file
# (actually located in $(top_srcdir)/util/) to lead Automake to believe that
# all sources are located in this directory.
UTILSOURCES = \
	util.c

opari_CXXFLAGS = -DOPARI_VT

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

nodist_opari_SOURCES = \
	$(UTILSOURCES)

$(UTILSOURCES):
	$(LN_S) $(top_srcdir)/util/$@

CLEANFILES = \
	$(UTILSOURCES)

EXTRA_DIST = \
	fixinc/cctype \
	fixinc/cstddef \
	fixinc/cstdio \
	fixinc/cstdlib \
	fixinc/cstring

