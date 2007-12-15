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

AM_CPPFLAGS = $(sds_portals_utcp_CPPFLAGS)

sources = \
        sds_portals_utcp.h \
        sds_portals_utcp_component.c \
        sds_portals_utcp_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sds_portals_utcp_DSO
component_noinst =
component_install = mca_sds_portals_utcp.la
else
component_noinst = libmca_sds_portals_utcp.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sds_portals_utcp_la_SOURCES = $(sources)
mca_sds_portals_utcp_la_LDFLAGS = -module -avoid-version $(sds_portals_utcp_LDFLAGS)
mca_sds_portals_utcp_la_LIBADD = $(sds_portals_utcp_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_sds_portals_utcp_la_SOURCES =$(sources)
libmca_sds_portals_utcp_la_LDFLAGS = -module -avoid-version $(sds_portals_utcp_LDFLAGS)
libmca_sds_portals_utcp_la_LIBADD = $(sds_portals_utcp_LIBS)
