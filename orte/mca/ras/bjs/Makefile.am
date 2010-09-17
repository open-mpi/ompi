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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_ras_bjs_DSO
component_noinst =
component_install = mca_ras_bjs.la
else
component_noinst = libmca_ras_bjs.la
component_install =
endif

AM_CPPFLAGS= $(ras_bjs_CPPFLAGS)

proxy_SOURCES = \
    ras_bjs.h \
    ras_bjs_component.c \
    ras_bjs.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_bjs_la_SOURCES = $(proxy_SOURCES)
mca_ras_bjs_la_LIBADD = $(ras_bjs_LIBS)
mca_ras_bjs_la_LDFLAGS = -module -avoid-version $(ras_bjs_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_bjs_la_SOURCES = $(proxy_SOURCES)
libmca_ras_bjs_la_LIBADD = $(ras_bjs_LIBS)
libmca_ras_bjs_la_LDFLAGS = -module -avoid-version $(ras_bjs_LDFLAGS)
