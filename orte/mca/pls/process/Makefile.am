#
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options


dist_pkgdata_DATA = help-orte-pls-process.txt

sources = \
        pls_process.h \
        pls_process_component.c \
        pls_process_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pls_process_DSO
component_noinst =
component_install = mca_pls_process.la
else
component_noinst = libmca_pls_process.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pls_process_la_SOURCES = $(sources)
mca_pls_process_la_LDFLAGS = -module -avoid-version
mca_pls_process_la_LIBADD = \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_pls_process_la_SOURCES =$(sources)
libmca_pls_process_la_LDFLAGS = -module -avoid-version
