INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfmerge
endif

otfmerge_LDADD = $(top_builddir)/otflib/libotf.la
otfmerge_DEPENDENCIES = $(otfmerge_LDADD)
otfmerge_SOURCES = \
	handler.h \
	hash.h \
	otfmerge.c \
	handler.c \
	hash.c

EXTRA_DIST = \
	otfmerge_vs08.vcproj
	
