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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_localhost_DSO
component_noinst =
component_install = mca_ras_localhost.la
else
component_noinst = libmca_ras_localhost.la
component_install =
endif

localhost_SOURCES = \
    ras_localhost.h \
    ras_localhost_module.c \
    ras_localhost_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_localhost_la_SOURCES = $(localhost_SOURCES)
mca_ras_localhost_la_LIBADD = \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_ras_localhost_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_localhost_la_SOURCES = $(localhost_SOURCES)
libmca_ras_localhost_la_LIBADD =
libmca_ras_localhost_la_LDFLAGS = -module -avoid-version
