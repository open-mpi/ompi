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
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_usnic_CPPFLAGS) 
AM_CFLAGS = $(btl_usnic_CFLAGS)

dist_pkgdata_DATA = \
    help-mpi-btl-usnic.txt

sources = \
    btl_usnic_compat.h \
    btl_usnic_module.c \
    btl_usnic_module.h \
    btl_usnic.h \
    btl_usnic_ack.c \
    btl_usnic_ack.h \
    btl_usnic_component.c \
    btl_usnic_endpoint.c \
    btl_usnic_endpoint.h \
    btl_usnic_frag.c \
    btl_usnic_frag.h \
    btl_usnic_hwloc.h \
    btl_usnic_mca.c \
    btl_usnic_proc.c \
    btl_usnic_proc.h \
    btl_usnic_recv.c \
    btl_usnic_recv.h \
    btl_usnic_send.c \
    btl_usnic_send.h \
    btl_usnic_stats.h \
    btl_usnic_stats.c \
    btl_usnic_util.c \
    btl_usnic_util.h

if OPAL_HAVE_HWLOC
sources += btl_usnic_hwloc.c
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_btl_usnic_DSO
lib =
lib_sources =
component = mca_btl_usnic.la
component_sources = $(sources)
else
lib = libmca_btl_usnic.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_usnic_la_SOURCES = $(component_sources)
mca_btl_usnic_la_LDFLAGS = -module -avoid-version $(btl_usnic_LDFLAGS) 
mca_btl_usnic_la_LIBADD = $(btl_usnic_LIBS) \
    $(top_ompi_builddir)/ompi/mca/common/verbs/libmca_common_verbs.la

noinst_LTLIBRARIES = $(lib)
libmca_btl_usnic_la_SOURCES = $(lib_sources)
libmca_btl_usnic_la_LDFLAGS= -module -avoid-version $(btl_usnic_LDFLAGS) 
libmca_btl_usnic_la_LIBADD = $(btl_usnic_LIBS)
