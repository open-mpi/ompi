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
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

sources = \
        coll_libnbc.h \
	coll_libnbc_component.c \
	coll_libnbc_ireduce_scatter_block.c \
	nbc.c \
	nbc_internal.h \
	libdict/dict.h \
	libdict/dict_private.h \
	libdict/dict.c \
	libdict/hb_tree.h \
	libdict/hb_tree.c \
	nbc_iallgather.c \
	nbc_iallgatherv.c \
	nbc_iallreduce.c \
	nbc_ialltoall.c \
	nbc_ialltoallv.c \
	nbc_ialltoallw.c \
	nbc_ibarrier.c \
	nbc_ibcast.c \
	nbc_ibcast_inter.c \
	nbc_iexscan.c \
	nbc_igather.c \
	nbc_igatherv.c \
	nbc_ineighbor_allgather.c \
	nbc_ineighbor_allgatherv.c \
	nbc_ineighbor_alltoall.c \
	nbc_ineighbor_alltoallv.c \
	nbc_ineighbor_alltoallw.c \
	nbc_ireduce.c \
	nbc_ireduce_scatter.c \
	nbc_iscan.c \
	nbc_iscatter.c \
	nbc_iscatterv.c \
	nbc_neighbor_helpers.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_libnbc_DSO
component_noinst =
component_install = mca_coll_libnbc.la
else
component_noinst = libmca_coll_libnbc.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_libnbc_la_SOURCES = $(sources)
mca_coll_libnbc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_libnbc_la_SOURCES =$(sources)
libmca_coll_libnbc_la_LDFLAGS = -module -avoid-version
