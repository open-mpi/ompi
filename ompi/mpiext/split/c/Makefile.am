#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = profile

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we need it to be 0

AM_CPPFLAGS     += -DOMPI_BUILD_MPI_PROFILING=0

# This file builds the C bindings for MPI extensions.  It must be
# present in all MPI extensions.

include $(top_srcdir)/Makefile.ompi-rules

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_split_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/ompi/mpiext/split/c

# This is the header file that is installed.
ompi_HEADERS = mpiext_split_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_split_c_la_SOURCES = 
if BUILD_MPI_BINDINGS_LAYER
libmpiext_split_c_la_SOURCES += \
        $(ompi_HEADERS) \
        mpiext_isplit_recv.c \
        mpiext_isplit_send.c \
        mpiext_split_recv.c \
        mpiext_split_send.c
endif

libmpiext_split_c_la_LDFLAGS = -module -avoid-version
libmpiext_split_c_la_LIBADD = profile/libpmpiext_split_c.la

# Man page installation
nodist_man_MANS = OMPI_Split_send.3

# Man page sources
EXTRA_DIST = $(nodist_man_MANS:.3=.3in) 

distclean-local:
	rm -f $(nodist_man_MANS)
