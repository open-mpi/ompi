AUTOMAKE_OPTIONS = -Wno-gnu

if AMBUILDBINARIES

WRAP_CC = vtcc
WRAP_CXX = vtcxx

if AMHAVEF77
WRAP_F77 = vtf77
else
WRAP_F77 =
endif

if AMHAVEFC
WRAP_FC = vtf90
else
WRAP_FC =
endif

endif # AMBUILDBINARIES

vtdata_DATA = \
	vtcc-wrapper-data.txt \
	vtcxx-wrapper-data.txt \
	vtf77-wrapper-data.txt \
	vtf90-wrapper-data.txt

vtdatadir = $(datadir)

bin_PROGRAMS = $(WRAP_CC) $(WRAP_CXX) $(WRAP_F77) $(WRAP_FC)

INCLUDES = -I$(top_builddir)/util/ -I$(top_srcdir)/vtlib/

CC = $(CC_FOR_BUILD)
CFLAGS = $(CFLAGS_FOR_BUILD)
CPP = $(CPP_FOR_BUILD)
CXX = $(CXX_FOR_BUILD)
CXXFLAGS = $(CXXFLAGS_FOR_BUILD)
CXXCPP = $(CXXCPP_FOR_BUILD)
CPPFLAGS = $(CPPFLAGS_FOR_BUILD)
LDFLAGS = $(LDFLAGS_FOR_BUILD)
LIBS = $(LIBS_FOR_BUILD)

vtcc_SOURCES = compwrap.cc compwrap.h $(top_srcdir)/util/util.c $(top_srcdir)/util/installdirs.c
vtcc_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_CC 

vtcxx_SOURCES = compwrap.cc compwrap.h
vtcxx_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_CXX
vtcxx_LDADD = util.$(OBJEXT) installdirs.$(OBJEXT)

vtf77_SOURCES = compwrap.cc compwrap.h
vtf77_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_F77
vtf77_LDADD = util.$(OBJEXT) installdirs.$(OBJEXT)

vtf90_SOURCES = compwrap.cc compwrap.h
vtf90_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_F90
vtf90_LDADD = util.$(OBJEXT) installdirs.$(OBJEXT)

