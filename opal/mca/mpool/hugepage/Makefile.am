#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(mpool_hugepage_CPPFLAGS)

sources = mpool_hugepage_module.c mpool_hugepage_component.c

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
opal_HEADERS = mpool_hugepage.h
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_mpool_hugepage_DSO
component_noinst =
component_install = mca_mpool_hugepage.la
else
component_noinst = libmca_mpool_hugepage.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_hugepage_la_SOURCES = $(sources)
mca_mpool_hugepage_la_LDFLAGS = -module -avoid-version
mca_mpool_hugepage_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(mpool_hugepage_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_hugepage_la_SOURCES = $(sources)
libmca_mpool_hugepage_la_LDFLAGS = -module -avoid-version
libmca_mpool_hugepage_la_LIBADD = $(mpool_hugepage_LIBS)
