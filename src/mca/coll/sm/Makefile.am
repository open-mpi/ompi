#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

EXTRA_DIST = VERSION

sources = \
        coll_sm.h \
        coll_sm.c \
        coll_sm_allgather.c \
        coll_sm_alltoall.c \
        coll_sm_barrier.c \
        coll_sm_bcast.c \
        coll_sm_component.c \
        coll_sm_gather.c \
        coll_sm_reduce.c \
        coll_sm_scatter.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_sm_DSO
component_noinst =
component_install = mca_coll_sm.la
else
component_noinst = libmca_coll_sm.la
component_install =
endif

common = $(top_ompi_builddir)/src/mca/common/sm/libmca_common_sm.la

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_sm_la_SOURCES = $(sources)
mca_coll_sm_la_LDFLAGS = -module -avoid-version
mca_coll_sm_la_LIBADD = $(common)

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_sm_la_SOURCES =$(sources)
libmca_coll_sm_la_LDFLAGS = -module -avoid-version
libmca_coll_sm_la_LIBADD = $(common)
