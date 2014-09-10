# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
# Copyright (c)      2012 Oak Rigde National Laboratory. All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# OMPI_PROFILING_DEFINES flag s enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory
# we definately need it to be 1.
#
AM_CPPFLAGS = -DOMPI_PROFILING_DEFINES=1

#
# This build needs to go through only if profiling is required.
# Further, this build HAS to go through if profiling is required.
#

noinst_LTLIBRARIES =
if BUILD_PMPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_pmpit.la
endif

headers = defines.h

nodist_libmpi_pmpit_la_SOURCES = \
	pcategory_changed.c \
	pcategory_get_categories.c \
	pcategory_get_cvars.c \
	pcategory_get_info.c \
	pcategory_get_index.c \
	pcategory_get_num.c \
	pcategory_get_pvars.c \
	pcvar_get_info.c \
	pcvar_get_index.c \
	pcvar_get_num.c \
	pcvar_handle_alloc.c \
	pcvar_handle_free.c \
	pcvar_read.c \
	pcvar_write.c \
	penum_get_info.c \
	penum_get_item.c \
	pfinalize.c \
	pinit_thread.c \
	ppvar_get_info.c \
	ppvar_get_index.c \
	ppvar_get_num.c \
	ppvar_handle_alloc.c \
	ppvar_handle_free.c \
	ppvar_read.c \
	ppvar_readreset.c \
	ppvar_reset.c \
	ppvar_session_create.c \
	ppvar_session_free.c \
	ppvar_start.c \
	ppvar_stop.c \
	ppvar_write.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_pmpit_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpi/tool/$$pname $@ ; \
	fi

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
ompi_HEADERS = $(headers)
endif

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_pmpit_la_SOURCES)

# Don't want these targets in here

tags-recursive:
tags:
TAGS:
GTAGS:
ID:
