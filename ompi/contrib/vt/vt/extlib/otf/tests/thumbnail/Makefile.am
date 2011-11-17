check_PROGRAMS = otf_thumbnail

otf_thumbnail_SOURCES = thumbnail.c
otf_thumbnail_DEPENDENCIES = $(top_builddir)/otfauxlib/libotfaux.la \
	$(top_builddir)/otflib/libotf.la
otf_thumbnail_LDADD = $(top_builddir)/otfauxlib/libotfaux.la \
	$(top_builddir)/otflib/libotf.la
otf_thumbnail_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib \
	-I$(top_srcdir)/otfauxlib
