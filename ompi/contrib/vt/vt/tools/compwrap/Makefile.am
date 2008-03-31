AUTOMAKE_OPTIONS = -Wno-gnu

if AMBUILDBINARIES

if AMHAVECC
WRAP_CC = vtcc
else
WRAP_CC =
endif

if AMHAVECXX
WRAP_CXX = vtcxx
else
WRAP_CXX =
endif

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

INCLUDES = -I$(top_srcdir)/vtlib/

CXX = $(CXX_FOR_BUILD)
CXXFLAGS = $(CXXFLAGS_FOR_BUILD)
CXXCPP = $(CXXCPP_FOR_BUILD)
CPPFLAGS = $(CPPFLAGS_FOR_BUILD)
LDFLAGS = $(LDFLAGS_FOR_BUILD)
LIBS = $(LIBS_FOR_BUILD)

AM_CXXFLAGS = -DDATADIR=\"$(datadir)\"

vtcc_SOURCES = compwrap.cc compwrap.h
vtcc_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_CC 

vtcxx_SOURCES = $(vtcc_SOURCES)
vtcxx_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_CXX

vtf77_SOURCES = $(vtcc_SOURCES)
vtf77_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_F77

vtf90_SOURCES = $(vtcc_SOURCES)
vtf90_CXXFLAGS = $(AM_CXXFLAGS) -DWRAP_LANG_F90

