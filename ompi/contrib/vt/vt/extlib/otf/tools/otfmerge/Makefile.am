INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfmerge
endif

otfmerge_LDADD = $(top_builddir)/otflib/libotf.la
if ! AMDEPZLIB
otfmerge_LDADD += $(ZLIB_LIB_LINE)
endif

otfmerge_SOURCES = \
	handler.h \
	hash.h \
	otfmerge.c \
	handler.c \
	hash.c

EXTRA_DIST = \
	otfmerge_vs05.vcproj
	
