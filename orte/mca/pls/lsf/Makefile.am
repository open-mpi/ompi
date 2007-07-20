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
# Copyright (c) 2007      Cisco, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(pls_lsf_CPPFLAGS)

dist_pkgdata_DATA = help-pls-lsf.txt

sources = \
        pls_lsf.h \
        pls_lsf_component.c \
        pls_lsf_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_lsf_DSO
lib =
lib_sources = 
component = mca_pls_lsf.la
component_sources = $(sources)
else
lib = libmca_pls_lsf.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_pls_lsf_la_SOURCES = $(component_sources)
mca_pls_lsf_la_LDFLAGS = -module -avoid-version $(pls_lsf_LDFLAGS)
mca_pls_lsf_la_LIBADD = \
    $(pls_lsf_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(lib)
libmca_pls_lsf_la_SOURCES = $(lib_sources)
libmca_pls_lsf_la_LDFLAGS = -module -avoid-version $(pls_lsf_LDFLAGS)
libmca_pls_lsf_la_LIBADD = $(pls_lsf_LIBS)
