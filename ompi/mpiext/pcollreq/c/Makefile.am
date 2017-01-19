#
# Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_pcollreq_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/ompi/mpiext/pcollreq/c

# This is the header file that is installed.
ompi_HEADERS = mpiext_pcollreq_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_pcollreq_c_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_pcollreq.c
libmpiext_pcollreq_c_la_LDFLAGS = -module -avoid-version
