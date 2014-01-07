#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file builds the mpif.h-based bindings for MPI extensions.  It
# is optional in MPI extensions.

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# Note that the mpif.h-based bindings are optional -- they can only be
# built if OMPI is also building the Fortran-based bindings.  So we
# initially set some Makefile macros to empty, and then conditionally
# add to them later.
noinst_LTLIBRARIES =

# Directory where the header file to be included in mpif-ext.h must be
# installed.
ompidir = $(ompiincludedir)/ompi/mpiext/example/mpif-h

# Just like noinst_LTLIBRARIES, set this macro to empty and
# conditionally add to it later.
ompi_HEADERS =

# Use the Automake conditional to know if we're building the mpif.h
# bindings.
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS

# If we are, build the convenience libtool library that will be
# slurped up into libmpi_mpifh.la.
noinst_LTLIBRARIES += libmpiext_example_mpifh.la

# This is the header file that is installed.
ompi_HEADERS += mpiext_example_mpifh.h

# Sources for the convenience libtool library.
libmpiext_example_mpifh_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_example_progress_f.c
libmpiext_example_mpifh_la_LDFLAGS = -module -avoid-version
endif
