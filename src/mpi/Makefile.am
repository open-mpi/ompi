# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_bindings.la

SUBDIRS = c cxx f77 f90

# See if configure found a valid f77 compiler

if BUILD_MPI_F77
f77_lib = f77/libmpi_f77.la
else
f77_lib =
endif

# See if configure found a valid f90/f95 compiler

if BUILD_MPI_F90
f90_lib =
else
f90_lib =
endif

libmpi_bindings_la_SOURCES =
libmpi_bindings_la_LIBADD = c/libmpi_c.la $(f77_lib) $(f90_lib)
