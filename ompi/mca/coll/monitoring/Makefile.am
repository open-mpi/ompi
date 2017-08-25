#
# Copyright (c) 2016 Inria.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

monitoring_sources  = \
	coll_monitoring.h            \
	coll_monitoring_allgather.c  \
	coll_monitoring_allgatherv.c \
	coll_monitoring_allreduce.c  \
	coll_monitoring_alltoall.c   \
	coll_monitoring_alltoallv.c  \
	coll_monitoring_alltoallw.c  \
	coll_monitoring_barrier.c    \
	coll_monitoring_bcast.c      \
	coll_monitoring_component.c  \
	coll_monitoring_exscan.c     \
	coll_monitoring_gather.c     \
	coll_monitoring_gatherv.c    \
	coll_monitoring_neighbor_allgather.c  \
	coll_monitoring_neighbor_allgatherv.c \
	coll_monitoring_neighbor_alltoall.c   \
	coll_monitoring_neighbor_alltoallv.c  \
	coll_monitoring_neighbor_alltoallw.c  \
	coll_monitoring_reduce.c     \
	coll_monitoring_reduce_scatter.c \
	coll_monitoring_reduce_scatter_block.c \
	coll_monitoring_scan.c       \
	coll_monitoring_scatter.c    \
	coll_monitoring_scatterv.c

if MCA_BUILD_ompi_coll_monitoring_DSO
component_noinst =
component_install = mca_coll_monitoring.la
else
component_noinst = libmca_coll_monitoring.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_monitoring_la_SOURCES = $(monitoring_sources)
mca_coll_monitoring_la_LDFLAGS = -module -avoid-version
mca_coll_monitoring_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(OMPI_TOP_BUILDDIR)/ompi/mca/common/monitoring/libmca_common_monitoring.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_monitoring_la_SOURCES = $(monitoring_sources)
libmca_coll_monitoring_la_LDFLAGS = -module -avoid-version
