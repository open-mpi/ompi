# Copyright (c) 2021 Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
#  Additional copyrights may follow
#
# $HEADER$
#
#

dist_oshmemdata_DATA = \
	help-oshmem-scoll-ucc.txt

AM_CPPFLAGS = $(scoll_ucc_CPPFLAGS)

scoll_ucc_sources =           \
		scoll_ucc.h           \
		scoll_ucc_debug.h     \
		scoll_ucc_dtypes.h    \
		scoll_ucc_common.h    \
		scoll_ucc_module.c    \
		scoll_ucc_component.c \
		scoll_ucc_barrier.c   \
		scoll_ucc_broadcast.c \
		scoll_ucc_reduce.c    \
		scoll_ucc_collect.c   \
		scoll_ucc_alltoall.c 
if MCA_BUILD_oshmem_scoll_ucc_DSO
component_noinst =
component_install = mca_scoll_ucc.la
else
component_noinst = libmca_scoll_ucc.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_scoll_ucc_la_SOURCES = $(scoll_ucc_sources)
mca_scoll_ucc_la_LIBADD = $(top_builddir)/oshmem/liboshmem.la \
	$(scoll_ucc_LIBS)
mca_scoll_ucc_la_LDFLAGS = -module -avoid-version $(scoll_ucc_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_scoll_ucc_la_SOURCES =$(scoll_ucc_sources)
libmca_scoll_ucc_la_LIBADD = $(scoll_ucc_LIBS)
libmca_scoll_ucc_la_LDFLAGS = -module -avoid-version $(scoll_ucc_LDFLAGS)
