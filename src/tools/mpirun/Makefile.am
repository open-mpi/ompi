# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

libs = $(top_builddir)/src/libmpi.la

bin_SCRIPTS = mpirun mpiboot

EXTRA_DIST = $(bin_SCRIPTS)

bin_PROGRAMS =	mpirun2

mpirun2_SOURCES = \
	mpirun2.cc
mpirun2_LDADD = \
	$(libs) \
	$(LIBMPI_EXTRA_LIBS) \
	$(LIBOMPI_EXTRA_LIBS)
mpirun2_LDFLAGS = \
	$(libs) \
	$(LIBMPI_EXTRA_LDFLAGS) \
	$(LIBOMPI_EXTRA_LDFLAGS)
mpirun2_DEPENDENCIES = \
	$(libs)

