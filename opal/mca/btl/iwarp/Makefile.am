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
# Copyright (c) 2007-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2011      Mellanox Technologies.  All rights reserved.
# Copyright (c) 2012      Oak Ridge National Laboratory.  All rights reserved
# Copyright (c) 2013      Intel, Inc. All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2018      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(btl_iwarp_CPPFLAGS)
AM_LFLAGS = -Pbtl_iwarp_ini_yy
LEX_OUTPUT_ROOT = lex.btl_iwarp_ini_yy

amca_paramdir = $(AMCA_PARAM_SETS_DIR)

dist_opaldata_DATA = \
        help-mpi-btl-iwarp.txt \
        connect/help-mpi-btl-iwarp-cpc-base.txt \
        mca-btl-iwarp-device-params.ini

sources = \
    btl_iwarp.c \
    btl_iwarp.h \
    btl_iwarp_component.c \
    btl_iwarp_endpoint.c \
    btl_iwarp_endpoint.h \
    btl_iwarp_frag.c \
    btl_iwarp_frag.h \
    btl_iwarp_proc.c \
    btl_iwarp_proc.h \
    btl_iwarp_eager_rdma.h \
    btl_iwarp_lex.h \
    btl_iwarp_lex.l \
    btl_iwarp_mca.c \
    btl_iwarp_mca.h \
    btl_iwarp_ini.c \
    btl_iwarp_ini.h \
    btl_iwarp_async.c \
    btl_iwarp_async.h \
    btl_iwarp_ip.h \
    btl_iwarp_ip.c \
    btl_iwarp_put.c \
    btl_iwarp_get.c \
    btl_iwarp_atomic.c \
    connect/base.h \
    connect/btl_iwarp_connect_base.c \
    connect/btl_iwarp_connect_empty.c \
    connect/btl_iwarp_connect_empty.h \
    connect/connect.h

# If we have rdmacm support, build that CPC
if MCA_btl_iwarp_have_rdmacm
sources += \
    connect/btl_iwarp_connect_rdmacm.c \
    connect/btl_iwarp_connect_rdmacm.h

dist_opaldata_DATA += connect/help-mpi-btl-iwarp-cpc-rdmacm.txt
endif


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_iwarp_DSO
lib =
lib_sources =
component = mca_btl_iwarp.la
component_sources = $(sources)
else
lib = libmca_btl_iwarp.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_iwarp_la_SOURCES = $(component_sources)
mca_btl_iwarp_la_LDFLAGS = -module -avoid-version $(btl_iwarp_LDFLAGS)
mca_btl_iwarp_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(btl_iwarp_LIBS) \
    $(OPAL_TOP_BUILDDIR)/opal/mca/common/verbs/lib@OPAL_LIB_PREFIX@mca_common_verbs.la
if OPAL_cuda_support
mca_btl_iwarp_la_LIBADD += \
    $(OPAL_TOP_BUILDDIR)/opal/mca/common/cuda/lib@OPAL_LIB_PREFIX@mca_common_cuda.la
endif

noinst_LTLIBRARIES = $(lib)
libmca_btl_iwarp_la_SOURCES = $(lib_sources)
libmca_btl_iwarp_la_LDFLAGS= -module -avoid-version $(btl_iwarp_LDFLAGS)
libmca_btl_iwarp_la_LIBADD = $(btl_iwarp_LIBS)

maintainer-clean-local:
	rm -f btl_iwarp_lex.c
