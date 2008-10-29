INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfconfig
endif

otfconfig_LDADD = $(top_builddir)/otflib/libotf.la
if ! AMDEPZLIB
otfconfig_LDADD += $(ZLIB_LIB_LINE)
endif

otfconfig_SOURCES = \
	otfconfig.c

AM_CFLAGS = -DOTFCONFIG_INCLUDEDIR=\"$(includedir)\" -DOTFCONFIG_LIBDIR=\"$(libdir)\"
