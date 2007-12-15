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

if OMPI_BUILD_rmgr_urm_DSO
component_noinst =
component_install = mca_rmgr_urm.la
else
component_noinst = libmca_rmgr_urm.la
component_install =
endif

proxy_SOURCES = \
    rmgr_urm.c \
    rmgr_urm.h \
    rmgr_urm_component.c

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmgr_urm_la_SOURCES = $(proxy_SOURCES)
mca_rmgr_urm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmgr_urm_la_SOURCES = $(proxy_SOURCES)
libmca_rmgr_urm_la_LDFLAGS = -module -avoid-version
