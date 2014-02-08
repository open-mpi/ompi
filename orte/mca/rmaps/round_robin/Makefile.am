#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-orte-rmaps-rr.txt

sources = \
        rmaps_rr.c \
        rmaps_rr.h \
        rmaps_rr_component.c \
        rmaps_rr_mappers.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rmaps_round_robin_DSO
component_noinst =
component_install = mca_rmaps_round_robin.la
else
component_noinst = libmca_rmaps_round_robin.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmaps_round_robin_la_SOURCES = $(sources)
mca_rmaps_round_robin_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmaps_round_robin_la_SOURCES =$(sources)
libmca_rmaps_round_robin_la_LDFLAGS = -module -avoid-version
