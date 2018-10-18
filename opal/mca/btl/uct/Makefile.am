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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2018 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(btl_uct_CPPFLAGS)

amca_paramdir = $(AMCA_PARAM_SETS_DIR)

sources = \
    btl_uct.h \
    btl_uct_module.c \
    btl_uct_component.c \
    btl_uct_rdma.h \
    btl_uct_rdma.c \
    btl_uct_endpoint.h \
    btl_uct_endpoint.c \
    btl_uct_amo.c \
    btl_uct_am.h \
    btl_uct_am.c \
    btl_uct_frag.h \
    btl_uct_frag.c \
    btl_uct_tl.c \
    btl_uct_types.h \
    btl_uct_device_context.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_uct_DSO
lib =
lib_sources =
component = mca_btl_uct.la
component_sources = $(sources)
else
lib = libmca_btl_uct.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_uct_la_SOURCES = $(component_sources)
mca_btl_uct_la_LDFLAGS = -module -avoid-version $(btl_uct_LDFLAGS)
mca_btl_uct_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la $(btl_uct_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_uct_la_SOURCES = $(lib_sources)
libmca_btl_uct_la_LDFLAGS = -module -avoid-version $(btl_uct_LDFLAGS)
libmca_btl_uct_la_LIBADD = $(btl_uct_LIBS)
