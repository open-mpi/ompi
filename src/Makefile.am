# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS		= include lam mpi tools

# If the --enable-single-library flag was given to configure, then the
# user wants to merge liblam and libmpi into a single big, honkin'
# libmpi.  So the libmpi in this directory should be a convenience
# (noinst) library.  Otherwise, it should be installed into lib.

if WANT_SINGLE_MPI_LIBRARY
install_lib		= libmpi.la
else
install_lib		=
endif

lib_LTLIBRARIES		= $(install_lib)

libmpi_la_SOURCES	=
libmpi_la_LIBADD	= \
			lam/liblam_convenience.la \
			mpi/libmpi_convenience.la
