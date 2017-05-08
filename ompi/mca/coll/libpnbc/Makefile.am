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

sources = \
        coll_libpnbc.h \
	coll_libpnbc_component.c \
	pnbc.c \
	pnbc_internal.h \
	libdict/dict.h \
	libdict/dict_private.h \
	libdict/dict.c \
	libdict/hb_tree.h \
	libdict/hb_tree.c \
	pnbc_iallgather.c \
	pnbc_iallgatherv.c \
	pnbc_iallreduce.c \
	pnbc_ialltoall.c \
	pnbc_ialltoallv.c \
	pnbc_ialltoallw.c \
	pnbc_ibarrier.c \
	pnbc_ibcast.c \
	pnbc_ibcast_inter.c \
	pnbc_iexscan.c \
	pnbc_igather.c \
	pnbc_igatherv.c \
	pnbc_ineighbor_allgather.c \
	pnbc_ineighbor_allgatherv.c \
	pnbc_ineighbor_alltoall.c \
	pnbc_ineighbor_alltoallv.c \
	pnbc_ineighbor_alltoallw.c \
	pnbc_ireduce.c \
	pnbc_ireduce_scatter.c \
	pnbc_ireduce_scatter_block.c \
	pnbc_iscan.c \
	pnbc_iscatter.c \
	pnbc_iscatterv.c \
	pnbc_neighbor_helpers.c \
	pnbc_start.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_libpnbc_DSO
component_noinst =
component_install = mca_coll_libpnbc.la
else
component_noinst = libmca_coll_libpnbc.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_libpnbc_la_SOURCES = $(sources)
mca_coll_libpnbc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_libpnbc_la_SOURCES =$(sources)
libmca_coll_libpnbc_la_LDFLAGS = -module -avoid-version
