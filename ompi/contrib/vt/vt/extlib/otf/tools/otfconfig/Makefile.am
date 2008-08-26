INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfconfig
endif

otfconfig_SOURCES = \
	otfconfig.c

AM_CFLAGS = -DOTFCONFIG_INCLUDEDIR=\"$(includedir)\" -DOTFCONFIG_LIBDIR=\"$(libdir)\"
