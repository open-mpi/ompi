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

AM_CPPFLAGS = $(pls_tbird_CPPFLAGS)

dist_pkgdata_DATA = help-pls-tbird.txt

sources = \
        pls_tbird.h \
        pls_tbird_component.c \
        pls_tbird_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_tbird_DSO
lib =
lib_sources = 
component = mca_pls_tbird.la
component_sources = $(sources)
else
lib = libmca_pls_tbird.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_pls_tbird_la_SOURCES = $(component_sources)
mca_pls_tbird_la_LDFLAGS = -module -avoid-version $(pls_tbird_LDFLAGS)
mca_pls_tbird_la_LIBADD = \
    $(pls_tbird_LIBS) \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la

noinst_LTLIBRARIES = $(lib)
libmca_pls_tbird_la_SOURCES = $(lib_sources)
libmca_pls_tbird_la_LDFLAGS = -module -avoid-version $(pls_tbird_LDFLAGS)
libmca_pls_tbird_la_LIBADD = $(pls_tbird_LIBS)
