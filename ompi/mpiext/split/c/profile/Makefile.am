# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
# Copyright (c) 2012      Oak Ridge National Laboratory. All rights reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# If OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we definately need it to be 1.

AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

#
# This build needs to go through only if profiling is required.
# Further, this build HAS to go through if profiling is required.
#

noinst_LTLIBRARIES = libpmpiext_split_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/ompi/mpiext/split/c

# This is the header file that is installed.

nodist_libpmpiext_split_c_la_SOURCES = \
        pmpiext_isplit_recv.c \
        pmpiext_isplit_send.c \
        pmpiext_split_send.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libpmpiext_split_c_la_SOURCES):
	$(OMPI_V_LN_S) if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpiext/split/c/$$pname $@ ; \
	fi

MAINTAINERCLEANFILES = $(nodist_libpmpiext_split_c_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
