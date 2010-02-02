INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfdump
endif

otfdump_LDADD = $(top_builddir)/otflib/libotf.la
otfdump_DEPENDENCIES = $(otfdump_LDADD)
otfdump_SOURCES = \
	Makefile.am \
	Handler.h \
	Handler.cpp \
	otfdump.cpp

EXTRA_DIST = \
	otfdump_vs08.vcproj

