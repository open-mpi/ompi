#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020-2021 Google, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(btl_sm_CPPFLAGS)

dist_opaldata_DATA = help-btl-sm.txt

libmca_btl_sm_la_sources = \
    btl_sm_module.c \
    btl_sm.h \
    btl_sm_component.c \
    btl_sm_fifo.h \
    btl_sm_frag.c \
    btl_sm_frag.h \
    btl_sm_send.c \
    btl_sm_sendi.c \
    btl_sm_fbox.h \
    btl_sm_get.c \
    btl_sm_put.c \
    btl_sm_types.h \
    btl_sm_virtual.h


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_sm_DSO
component_noinst =
component_install = mca_btl_sm.la
else
component_noinst = libmca_btl_sm.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_sm_la_SOURCES = $(libmca_btl_sm_la_sources)
mca_btl_sm_la_LDFLAGS = -module -avoid-version $(btl_sm_LDFLAGS)
mca_btl_sm_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(btl_sm_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_sm_la_SOURCES = $(libmca_btl_sm_la_sources)
libmca_btl_sm_la_LIBADD = $(btl_sm_LIBS)
libmca_btl_sm_la_LDFLAGS = -module -avoid-version $(btl_sm_LDFLAGS)
