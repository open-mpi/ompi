# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# See if configure found a valid f77 compiler

if BUILD_MPI_F77
f77_subdir	= f77
f77_lib		= f77/libmpi_f77.la
else
f77_subdir	=
f77_lib		=
endif

# See if configure found a valid f90/f95 compiler

if BUILD_MPI_F90
f90_subdir	= f90
f90_lib		= f90/libmpi_f90.la
else
f90_subdir	=
f90_lib		=
endif

SUBDIRS		= c cxx $(f77_subdir) $(f90_subdir)
DIST_SUBDIRS	= c cxx f77 f90

# If the --enable-single-library flag was given to configure, then the
# user wants to merge liblam and libmpi into a single big, honkin'
# libmpi.  So the libmpi in this directory should be a convenience
# (noinst) library.  Otherwise, it should be installed into lib.

if WANT_SINGLE_MPI_LIBRARY
install_lib		=
convenience_lib		= libmpi_convenience.la
else
install_lib		= libmpi.la
convenience_lib		=
endif

lib_LTLIBRARIES		= $(install_lib)
noinst_LTLIBRARIES	= $(convenience_lib)

sources			= c/libmpi_c.la $(f77_lib) $(f90_lib)

libmpi_la_SOURCES	=
libmpi_la_LIBADD	= $(sources)

libmpi_convenience_la_SOURCES =
libmpi_convenience_la_LIBADD = $(sources)
