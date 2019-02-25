#
# Copyright (c) 2018-2019 FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Convenience libtool library that will be slurped up into libmpi_mpifh.la.
noinst_LTLIBRARIES =

# Directory where the header file to be included in mpif-ext.h must be
# installed.
ompidir = $(ompiincludedir)/mpiext

# If we are, build the convenience libtool library that will be
# slurped up into libmpi_mpifh.la.
noinst_LTLIBRARIES += libmpiext_shortfloat_mpifh.la

# Just like noinst_LTLIBRARIES, set this macro to empty and
# conditionally add to it later.
ompi_HEADERS =

# Use the Automake conditional to know if we're building the mpif.h
# bindings.
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS

# This is the header file that is installed.
ompi_HEADERS += mpiext_shortfloat_mpifh.h

# Sources for the convenience libtool library.
libmpiext_shortfloat_mpifh_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_shortfloat_mpifh.c
libmpiext_shortfloat_mpifh_la_LDFLAGS = -module -avoid-version

endif
