# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = communicator datatype interface

# If the --enable-single-library flag was given to configure, then the
# user wants to merge liblam and libmpi into a single big, honkin'
# libmpi.  So the libmpi in this directory should be a convenience
# (noinst) library.  Otherwise, it should be installed into lib.

if WANT_SINGLE_MPI_LIBRARY
install_lib =
convenience_lib = libmpi_convenience.la
else
install_lib = libmpi.la
convenience_lib =
endif

lib_LTLIBRARIES = $(install_lib)
noinst_LTLIBRARIES = $(convenience_lib)

# To be added shortly
#        communicator/libmpi_communicator.la \
sources = \
        datatype/libmpi_datatype.la \
        interface/libmpi_interface.la

libmpi_la_SOURCES =
libmpi_la_LIBADD = $(sources)

libmpi_convenience_la_SOURCES =
libmpi_convenience_la_LIBADD = $(sources)
