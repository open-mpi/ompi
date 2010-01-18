INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfaux
endif

otfaux_LDADD = $(top_builddir)/otflib/libotf.la
otfaux_DEPENDENCIES = $(otfaux_LDADD)
otfaux_SOURCES = \
	Control.h \
	Handler.h \
	State.h \
	otfaux.cpp \
	Control.cpp \
	Handler.cpp \
	State.cpp

EXTRA_DIST = \
	otfaux_vs08.vcproj

