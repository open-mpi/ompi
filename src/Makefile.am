# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = include lam mpi mca . tools

# If the --enable-single-library flag was given to configure, then the
# user wants to merge liblam and libmpi into a single big, honkin'
# libmpi.  So the libmpi in this directory should be a convenience
# (noinst) library.  Otherwise, it should be installed into lib.

lam_sources = lam/liblam_convenience.la mca/libmca_lam.la
mpi_sources = mpi/libmpi_convenience.la mca/libmca_mpi.la

if WANT_SINGLE_MPI_LIBRARY
install_libs = libmpi.la
libmpi_sources = $(mpi_sources) $(lam_sources)
liblam_sources =
else
install_libs = libmpi.la liblam.la
libmpi_sources = $(mpi_sources)
liblam_sources = $(lam_sources)
endif

lib_LTLIBRARIES = $(install_libs)

libmpi_la_SOURCES =
libmpi_la_LIBADD = $(libmpi_sources)

liblam_la_SOURCES =
liblam_la_LIBADD = $(liblam_sources)
