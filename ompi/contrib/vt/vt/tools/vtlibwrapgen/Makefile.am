if AMBUILDBINARIES
bin_PROGRAMS = vtlibwrapgen
endif

AM_CPPFLAGS = $(CTOOLINCDIR) -I$(top_srcdir)/include -I$(top_builddir)/include -I$(top_srcdir)/util -I$(top_builddir)/util

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
# enable this option, so we have to sym link in the following sources (actually
# located in $(top_srcdir)/util/) to lead Automake to believe that all sources
# are located in this directory.
UTILSOURCES = \
	installdirs.c \
	util.c

vtlibwrapgen_SOURCES = \
	vt_libwrapgen.h \
	vt_libwrapgen_filter.h \
	vt_libwrapgen_parser.h \
	vt_libwrapgen.cc \
	vt_libwrapgen_filter.cc \
	vt_libwrapgen_parser.cc

nodist_vtlibwrapgen_SOURCES = \
	$(UTILSOURCES)

vtlibwrapgen_LDADD = \
	$(CTOOLLIBDIR) $(CTOOLLIB)

# Sym link in util sources to keep the non-subdir-objects behavior
$(UTILSOURCES):
	$(LN_S) $(top_srcdir)/util/$@

CLEANFILES = \
	$(UTILSOURCES)

