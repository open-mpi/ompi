#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      NVIDIA, Inc. All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This file builds the C bindings for MPI extensions.  It must be
# present in all MPI extensions.

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

include $(top_srcdir)/Makefile.ompi-rules

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_cuda_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/mpiext

# This is the header file that is installed.
nodist_ompi_HEADERS = mpiext_cuda_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_cuda_c_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_cuda.c
libmpiext_cuda_c_la_LDFLAGS = -module -avoid-version

# Man page installation
nodist_man_MANS = MPIX_Query_cuda_support.3

# Man page sources
EXTRA_DIST = $(nodist_man_MANS:.3=.3in)

distclean-local:
	rm -f $(nodist_man_MANS)
