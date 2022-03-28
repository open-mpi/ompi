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
# Copyright (c) 2009-2019 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2018      Intel, inc. All rights reserved
# Copyright (c) 2020      Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#dist_opaldata_DATA = help-mpi-btl-ofi.txt

AM_CPPFLAGS = $(btl_ofi_CPPFLAGS)
sources = \
    btl_ofi.h \
    btl_ofi_component.c \
    btl_ofi_endpoint.h \
    btl_ofi_endpoint.c \
    btl_ofi_module.c \
    btl_ofi_rdma.h \
    btl_ofi_rdma.c \
    btl_ofi_atomics.c \
    btl_ofi_frag.c \
    btl_ofi_frag.h \
    btl_ofi_context.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_ofi_DSO
lib =
lib_sources =
component = mca_btl_ofi.la
component_sources = $(sources)
else
lib = libmca_btl_ofi.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_ofi_la_SOURCES = $(component_sources)
mca_btl_ofi_la_LDFLAGS = -module -avoid-version \
                         $(btl_ofi_LDFLAGS)
mca_btl_ofi_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
    $(OPAL_TOP_BUILDDIR)/opal/mca/common/ofi/lib@OPAL_LIB_NAME@mca_common_ofi.la \
    $(btl_ofi_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_ofi_la_SOURCES = $(lib_sources)
libmca_btl_ofi_la_LDFLAGS = -module -avoid-version $(btl_ofi_LDFLAGS)
libmca_btl_ofi_la_LIBS = $(btl_ofi_LIBS)
