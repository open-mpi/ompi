INCLUDES = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

noinst_PROGRAMS = \
	otfdump

otfdump_LDADD = $(top_builddir)/otflib/libotf.la
if ! AMDEPZLIB
otfdump_LDADD += $(ZLIB_LIB_LINE)
endif

otfdump_SOURCES = \
	Makefile.am \
	Handler.h \
	Handler.cpp \
	otfdump.cpp

EXTRA_DIST = \
	otfdump_vs05.vcproj

