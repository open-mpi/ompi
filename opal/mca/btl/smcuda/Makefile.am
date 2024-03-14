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
# Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA = help-mpi-btl-smcuda.txt

libmca_btl_smcuda_la_sources = \
    btl_smcuda.c \
    btl_smcuda.h \
    btl_smcuda_component.c \
    btl_smcuda_endpoint.h \
    btl_smcuda_fifo.h \
    btl_smcuda_frag.c \
    btl_smcuda_frag.h \
    btl_smcuda_accelerator.c \
    btl_smcuda_accelerator.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_smcuda_DSO
component_noinst =
component_install = mca_btl_smcuda.la
else
component_noinst = libmca_btl_smcuda.la
component_install =
endif

# See opal/mca/common/cuda/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_smcuda_la_SOURCES = $(libmca_btl_smcuda_la_sources)
mca_btl_smcuda_la_LDFLAGS = -module -avoid-version $(btl_smcuda_LDFLAGS)
mca_btl_smcuda_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
    $(OPAL_TOP_BUILDDIR)/opal/mca/common/sm/lib@OPAL_LIB_NAME@mca_common_sm.la \
    $(btl_smcuda_LIBS)
mca_btl_smcuda_la_CPPFLAGS = $(btl_smcuda_CPPFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_smcuda_la_SOURCES = $(libmca_btl_smcuda_la_sources)
libmca_btl_smcuda_la_LDFLAGS = -module -avoid-version $(btl_smcuda_LDFLAGS)
libmca_btl_smcuda_la_CPPFLAGS = $(btl_smcuda_CPPFLAGS)
libmca_btl_smcuda_la_LIBADD = $(btl_smcuda_LIBS)
