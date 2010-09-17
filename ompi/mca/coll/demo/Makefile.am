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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Source files

sources = \
        coll_demo.h \
        coll_demo_allgather.c \
        coll_demo_allgatherv.c \
        coll_demo_allreduce.c \
        coll_demo_alltoall.c \
        coll_demo_alltoallv.c \
        coll_demo_alltoallw.c \
        coll_demo_barrier.c \
        coll_demo_bcast.c \
        coll_demo_component.c \
        coll_demo_gather.c \
        coll_demo_gatherv.c \
        coll_demo_module.c \
        coll_demo_reduce.c \
        coll_demo_reduce_scatter.c \
        coll_demo_scan.c \
        coll_demo_exscan.c \
        coll_demo_scatter.c \
        coll_demo_scatterv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_coll_demo_DSO
component_noinst =
component_install = mca_coll_demo.la
else
component_noinst = libmca_coll_demo.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_demo_la_SOURCES = $(sources)
mca_coll_demo_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_demo_la_SOURCES = $(sources)
libmca_coll_demo_la_LDFLAGS = -module -avoid-version
