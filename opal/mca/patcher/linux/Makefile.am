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
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if MCA_BUILD_opal_patcher_linux_DSO
component_noinst =
component_install = mca_patcher_linux.la
else
component_noinst = libmca_patcher_linux.la
component_install =
endif

linux_SOURCES = \
    patcher_linux.h \
    patcher_linux_module.c \
    patcher_linux_component.c

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_patcher_linux_la_SOURCES = $(linux_SOURCES)
nodist_mca_patcher_linux_la_SOURCES = $(linux_nodist_SOURCES)
mca_patcher_linux_la_LDFLAGS = -module -avoid-version
mca_patcher_linux_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_patcher_linux_la_SOURCES = $(linux_SOURCES)
nodist_libmca_patcher_linux_la_SOURCES = $(linux_nodist_SOURCES)
libmca_patcher_linux_la_LIBADD = $(patcher_linux_LIBS)
libmca_patcher_linux_la_LDFLAGS = -module -avoid-version
