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
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_ompidata_DATA = help-mpi-coll-smdirect.txt

not_used_yet = \
        coll_smdirect_allgather.c \
        coll_smdirect_allgatherv.c \
        coll_smdirect_alltoall.c \
        coll_smdirect_alltoallv.c \
        coll_smdirect_alltoallw.c \
        coll_smdirect_gather.c \
        coll_smdirect_gatherv.c \
        coll_smdirect_reduce_scatter.c \
        coll_smdirect_scan.c \
        coll_smdirect_exscan.c \
        coll_smdirect_scatter.c \
        coll_smdirect_allreduce.c \
        coll_smdirect_barrier.c \
        coll_smdirect_bcast.c \
        coll_smdirect_scatterv.c

sources = \
        coll_smdirect.h \
        coll_smdirect_component.c \
        coll_smdirect_module.c \
        coll_smdirect_reduce.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_coll_sm_DSO
component_install += mca_coll_smdirect.la
else
component_noinst += libmca_coll_smdirect.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_smdirect_la_SOURCES = $(sources)
mca_coll_smdirect_la_LDFLAGS = -module -avoid-version
mca_coll_smdirect_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(OMPI_TOP_BUILDDIR)/opal/mca/common/smdirect/lib@OPAL_LIB_NAME@mca_common_smdirect.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_smdirect_la_SOURCES =$(sources)
libmca_coll_smdirect_la_LDFLAGS = -module -avoid-version
