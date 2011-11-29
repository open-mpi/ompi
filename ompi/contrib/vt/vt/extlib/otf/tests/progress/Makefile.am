check_PROGRAMS = otf_progress

otf_progress_SOURCES = progress.c
otf_progress_DEPENDENCIES = $(top_builddir)/otflib/libotf.la
otf_progress_LDADD = $(top_builddir)/otflib/libotf.la
otf_progress_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib

clean-local:
	$(RM) *.otf *.def *.events *.marker *.otf.z *.def.z *.events.z *.marker.z
