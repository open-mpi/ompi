INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfprofile
endif

if AMHAVEOMP
otfprofile_CXXFLAGS = $(OPENMP_CXXFLAGS)
otfprofile_LDFLAGS = $(OPENMP_CXXFLAGS)
endif

otfprofile_LDADD = $(top_builddir)/otflib/libotf.la $(MATHLIB)
otfprofile_DEPENDENCIES = $(top_builddir)/otflib/libotf.la
otfprofile_SOURCES = \
	CSVParse.h \
	DataStructure.h \
	Definitions.h \
	Handler.h \
	Prodtex.h \
	Summary.h \
	otfprofile.cpp \
	CSVParse.cpp \
	DataStructure.cpp \
	Handler.cpp \
	Prodtex.cpp \
	Summary.cpp

