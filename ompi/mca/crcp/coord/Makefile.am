#
# Copyright (c) 2004-2007 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

dist_pkgdata_DATA = help-ompi-crcp-coord.txt

sources = \
        crcp_coord.h \
        crcp_coord_pml.h \
        crcp_coord_component.c \
        crcp_coord_module.c \
        crcp_coord_pml.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_crcp_coord_DSO
component_noinst =
component_install = mca_crcp_coord.la
else
component_noinst = libmca_crcp_coord.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_crcp_coord_la_SOURCES = $(sources)
mca_crcp_coord_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_crcp_coord_la_SOURCES = $(sources)
libmca_crcp_coord_la_LDFLAGS = -module -avoid-version
