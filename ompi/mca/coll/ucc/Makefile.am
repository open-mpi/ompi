# -*- shell-script -*-
#
#
# Copyright (c) 2021 Mellanox Technologies. All rights reserved.
# Copyright (c) 2022 NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
#

AM_CPPFLAGS = $(coll_ucc_CPPFLAGS)

coll_ucc_sources =                       \
		coll_ucc.h                       \
		coll_ucc_debug.h                 \
		coll_ucc_dtypes.h                \
		coll_ucc_common.h                \
		coll_ucc_module.c                \
		coll_ucc_component.c             \
		coll_ucc_allgather.c             \
		coll_ucc_allgatherv.c            \
		coll_ucc_allreduce.c             \
		coll_ucc_alltoall.c              \
		coll_ucc_alltoallv.c             \
		coll_ucc_barrier.c               \
		coll_ucc_bcast.c                 \
		coll_ucc_gather.c                \
		coll_ucc_gatherv.c               \
		coll_ucc_reduce.c                \
		coll_ucc_reduce_scatter_block.c  \
		coll_ucc_reduce_scatter.c        \
		coll_ucc_scatter.c               \
		coll_ucc_scatterv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_ucc_DSO
component_noinst =
component_install = mca_coll_ucc.la
else
component_noinst = libmca_coll_ucc.la
component_install =
endif

mcacomponentdir          = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_ucc_la_SOURCES  = $(coll_ucc_sources)
mca_coll_ucc_la_LIBADD   = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	                     $(coll_ucc_LIBS)
mca_coll_ucc_la_LDFLAGS  = -module -avoid-version $(coll_ucc_LDFLAGS)

noinst_LTLIBRARIES         = $(component_noinst)
libmca_coll_ucc_la_SOURCES = $(coll_ucc_sources)
libmca_coll_ucc_la_LIBADD  = $(coll_ucc_LIBS)
libmca_coll_ucc_la_LDFLAGS = -module -avoid-version $(coll_ucc_LDFLAGS)
