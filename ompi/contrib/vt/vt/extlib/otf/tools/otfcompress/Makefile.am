INCLUDES = \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib \
	$(ZLIB_INCLUDE_LINE)

if AMBUILDBINARIES
bin_PROGRAMS = otfcompress

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f otfdecompress; \
	$(LN_S) otfcompress$(EXEEXT) otfdecompress$(EXEEXT))
endif

otfcompress_LDADD = $(top_builddir)/otflib/libotf.la
if ! AMDEPZLIB
otfcompress_LDADD += $(ZLIB_LIB_LINE)
endif

otfcompress_SOURCES = \
	sys_time.h \
	otfcompress.c

EXTRA_DIST = \
	otfcompress_vs05.vcproj

