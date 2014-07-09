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
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(btl_usnic_CPPFLAGS) 
AM_CFLAGS = $(btl_usnic_CFLAGS)

EXTRA_DIST = README.txt README.test

dist_ompidata_DATA = \
    help-mpi-btl-usnic.txt

test_sources = \
    test/btl_usnic_component_test.h \
    test/btl_usnic_graph_test.h

libnl_utils_sources = \
    btl_usnic_libnl_utils.h

if OMPI_BTL_USNIC_BUILD_LIBNL1_UTILS
libnl_utils_sources += btl_usnic_libnl1_utils.c
endif OMPI_BTL_USNIC_BUILD_LIBNL1_UTILS

if OMPI_BTL_USNIC_BUILD_LIBNL3_UTILS
libnl_utils_sources += btl_usnic_libnl3_utils.c
endif OMPI_BTL_USNIC_BUILD_LIBNL3_UTILS

sources = \
    btl_usnic_compat.h \
    btl_usnic_module.c \
    btl_usnic_module.h \
    btl_usnic.h \
    btl_usnic_ack.c \
    btl_usnic_ack.h \
    btl_usnic_component.c \
    btl_usnic_connectivity.h \
    btl_usnic_cclient.c \
    btl_usnic_cagent.c \
    btl_usnic_endpoint.c \
    btl_usnic_endpoint.h \
    btl_usnic_ext.h \
    btl_usnic_ext.c \
    btl_usnic_frag.c \
    btl_usnic_frag.h \
    btl_usnic_graph.h \
    btl_usnic_graph.c \
    btl_usnic_hwloc.h \
    btl_usnic_map.c \
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
    btl_usnic_util.h \
    btl_usnic_test.c \
    btl_usnic_test.h \
    $(test_sources) \
    $(libnl_utils_sources)

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

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_usnic_la_SOURCES = $(component_sources)
mca_btl_usnic_la_LDFLAGS = -module -avoid-version $(btl_usnic_LDFLAGS) 
mca_btl_usnic_la_LIBADD = $(btl_usnic_LIBS) \
    $(top_ompi_builddir)/ompi/mca/common/verbs/libmca_common_verbs.la

noinst_LTLIBRARIES = $(lib)
libmca_btl_usnic_la_SOURCES = $(lib_sources)
libmca_btl_usnic_la_LDFLAGS= -module -avoid-version $(btl_usnic_LDFLAGS) 
libmca_btl_usnic_la_LIBADD = $(btl_usnic_LIBS)

if OMPI_BTL_USNIC_BUILD_UNIT_TESTS
ompi_btl_usnic_run_tests_SOURCES = test/ompi_btl_usnic_run_tests.c
ompi_btl_usnic_run_tests_LDADD = -ldl
bin_PROGRAMS = ompi_btl_usnic_run_tests
endif OMPI_BTL_USNIC_BUILD_UNIT_TESTS
