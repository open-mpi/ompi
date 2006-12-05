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

AM_CPPFLAGS = $(btl_openib_CPPFLAGS) -DPKGDATADIR=\"$(pkgdatadir)\"
AM_LFLAGS = -Pbtl_openib_ini_yy
LEX_OUTPUT_ROOT = lex.btl_openib_ini_yy

dist_pkgdata_DATA = \
        help-mpi-btl-openib.txt \
        mca-btl-openib-hca-params.ini

sources = \
    btl_openib.c \
    btl_openib.h \
    btl_openib_component.c \
    btl_openib_endpoint.c \
    btl_openib_endpoint.h \
    btl_openib_frag.c \
    btl_openib_frag.h \
    btl_openib_proc.c \
    btl_openib_proc.h \
    btl_openib_eager_rdma.h \
    btl_openib_lex.h \
    btl_openib_lex.l \
    btl_openib_mca.c \
    btl_openib_mca.h \
    btl_openib_ini.c \
    btl_openib_ini.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_btl_openib_DSO
lib =
lib_sources =
component = mca_btl_openib.la
component_sources = $(sources)
else
lib = libmca_btl_openib.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_btl_openib_la_SOURCES = $(component_sources)
mca_btl_openib_la_LDFLAGS = -module -avoid-version $(btl_openib_LDFLAGS) 
mca_btl_openib_la_LIBADD = \
    $(btl_openib_LIBS) \
    $(top_ompi_builddir)/ompi/libmpi.la \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(lib)
libmca_btl_openib_la_SOURCES = $(lib_sources)
libmca_btl_openib_la_LDFLAGS= -module -avoid-version $(btl_openib_LDFLAGS) 
libmca_btl_openib_la_LIBADD=$(btl_openib_LIBS)
