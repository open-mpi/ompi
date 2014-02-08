#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file builds the C bindings for MPI extensions.  It must be
# present in all MPI extensions.

# Example program
example: example.c
	mpicc example.c -o example -g

CLEANFILES = example

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

include $(top_srcdir)/Makefile.man-page-rules

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_affinity_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/ompi/mpiext/affinity/c

# This is the header file that is installed.
ompi_HEADERS = mpiext_affinity_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_affinity_c_la_SOURCES = \
        $(ompi_HEADERS) \
        mpiext_affinity_str.c
libmpiext_affinity_c_la_LDFLAGS = -module -avoid-version

# Man page installation
nodist_man_MANS = OMPI_Affinity_str.3

# Man page sources
EXTRA_DIST = $(nodist_man_MANS:.3=.3in) example.c
