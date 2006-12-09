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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options



CFLAGS = $(btl_mvapi_CFLAGS)
AM_CPPFLAGS = $(btl_mvapi_CPPFLAGS)

dist_pkgdata_DATA=help-mpi-btl-mvapi.txt

sources = \
    btl_mvapi.c \
    btl_mvapi.h \
    btl_mvapi_component.c \
    btl_mvapi_endpoint.c \
    btl_mvapi_endpoint.h \
    btl_mvapi_frag.c \
    btl_mvapi_frag.h \
    btl_mvapi_proc.c \
    btl_mvapi_proc.h \
    btl_mvapi_eager_rdma.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_mvapi_DSO
lib =
lib_sources =
component = mca_btl_mvapi.la
component_sources = $(sources)
else
lib = libmca_btl_mvapi.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_btl_mvapi_la_SOURCES = $(component_sources)
mca_btl_mvapi_la_LDFLAGS = -module -avoid-version $(btl_mvapi_LDFLAGS)
mca_btl_mvapi_la_LIBADD = \
    $(btl_mvapi_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
        
noinst_LTLIBRARIES = $(lib)
libmca_btl_mvapi_la_SOURCES = $(lib_sources)
libmca_btl_mvapi_la_LDFLAGS = -module -avoid-version$ $(btl_mvapi_LDFLAGS)
libmca_btl_mvapi_la_LIBADD = $(btl_mvapi_LIBS)
