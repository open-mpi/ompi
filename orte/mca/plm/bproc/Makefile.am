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
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA = help-plm-bproc.txt

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_plm_bproc_DSO
component_noinst =
component_install = mca_plm_bproc.la
else
component_noinst = libmca_plm_bproc.la
component_install =
endif

sources = \
    plm_bproc.h \
    plm_bproc_component.c \
    plm_bproc.c \
    plm_bproc_state.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_plm_bproc_la_SOURCES = $(sources)
mca_plm_bproc_la_LIBADD = $(plm_bproc_LIBS)
mca_plm_bproc_la_LDFLAGS = -module -avoid-version $(plm_bproc_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_plm_bproc_la_SOURCES = $(sources)
libmca_plm_bproc_la_LIBADD = $(plm_bproc_LIBS)
libmca_plm_bproc_la_LDFLAGS = -module -avoid-version $(plm_bproc_LDFLAGS)
