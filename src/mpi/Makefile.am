# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmpi_bindings.la

# See if configure found a valid f77 compiler

if OMPI_WANT_F77_BINDINGS
f77_lib = f77/libmpi_f77.la
else
f77_lib =
endif

# See if configure found a valid f90/f95 compiler

if OMPI_WANT_F90_BINDINGS
f90_lib =
else
f90_lib =
endif

if WANT_MPI_CXX_BINDINGS
cxx_lib = 
else
cxx_lib =
endif

SUBDIRS = c cxx f77 f90

libmpi_bindings_la_SOURCES =
libmpi_bindings_la_LIBADD = c/libmpi_c.la $(f77_lib) $(f90_lib) $(cxx_lib)
