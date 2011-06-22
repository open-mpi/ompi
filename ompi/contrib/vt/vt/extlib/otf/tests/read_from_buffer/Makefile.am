check_PROGRAMS = otf_read_from_buffer

otf_read_from_buffer_SOURCES = read_from_buffer.c
otf_read_from_buffer_DEPENDENCIES = $(top_builddir)/otflib/libotf.la
otf_read_from_buffer_LDADD = $(top_builddir)/otflib/libotf.la
otf_read_from_buffer_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib
