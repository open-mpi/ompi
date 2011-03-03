#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

AM_CPPFLAGS = $(btl_wv_CPPFLAGS)

dist_pkgdata_DATA = help-mpi-btl-wv.txt \
        mca-btl-wv-device-params.ini \
        connect/help-mpi-btl-wv-cpc-base.txt

sources = \
    btl_wv.c \
    btl_wv.h \
    btl_wv_component.c \
    btl_wv_def.h \
    btl_wv_eager_rdma.h \
    btl_wv_endpoint.c \
    btl_wv_endpoint.h \
    btl_wv_frag.c \
    btl_wv_frag.h \
    btl_wv_ini.c \
    btl_wv_ini.h \
    btl_wv_lex.h \
    btl_wv_lex.l \
    btl_wv_mca.c \
    btl_wv_mca.h \
    btl_wv_proc.c \
    btl_wv_proc.h \
    connect/base.h \
    connect/btl_wv_connect_base.c \
    connect/btl_wv_connect_oob.c \
    connect/btl_wv_connect_oob.h \
    connect/btl_wv_connect_empty.c \
    connect/btl_wv_connect_empty.h \
    connect/connect.h

    # Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_btl_wv_DSO
lib =
lib_sources = 
component = mca_btl_wv.la
component_sources = $(sources)
else
lib = libmca_btl_wv.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_wv_la_SOURCES = $(component_sources)
mca_btl_wv_la_LDFLAGS = -module -avoid-version $(btl_wv_LDFLAGS)
mca_btl_wv_la_LIBADD = $(btl_wv_LIBS)

noinst_LTLIBRARIES = $(lib)
libmca_btl_wv_la_SOURCES = $(lib_sources)
libmca_btl_wv_la_LDFLAGS = -module -avoid-version $(btl_wv_LDFLAGS)
libmca_btl_wv_la_LIBADD = $(btl_wv_LIBS)
