INCLUDES = \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib \
	-I$(top_srcdir)/otfauxlib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfaux
endif

otfaux_LDADD = \
	$(top_builddir)/otfauxlib/libotfaux.la \
	$(top_builddir)/otflib/libotf.la \
	$(MATHLIB)
otfaux_DEPENDENCIES = $(otfaux_LDADD)
otfaux_SOURCES = \
	Control.h \
	Handler.h \
	Stats.h \
	otfaux.cpp \
	Control.cpp \
	Handler.cpp \
	Stats.cpp

EXTRA_DIST = \
	otfaux_vs08.vcproj
