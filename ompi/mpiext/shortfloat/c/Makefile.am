#
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_shortfloat_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/mpiext

# This is the header file that is installed.
ompi_HEADERS = mpiext_shortfloat_c.h

# Sources for the convenience libtool library.
libmpiext_shortfloat_c_la_SOURCES = \
    $(ompi_HEADERS) \
    mpiext_shortfloat_c_dummy.c
libmpiext_shortfloat_c_la_LDFLAGS = -module -avoid-version
