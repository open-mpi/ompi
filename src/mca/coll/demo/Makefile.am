#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Need to use our own AUTOMAKE_OPTIONS -- cannot include
# Makefile.options from main ompi tree because Automake now defers
# "include" statements until "make" time.  Hence, if we're building
# this component outside the ompi tree, the include statement will
# fail.

AUTOMAKE_OPTIONS	= foreign dist-bzip2


EXTRA_DIST = VERSION

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_coll_demo_DSO
component_noinst =
component_install = mca_coll_demo.la
else
component_noinst = libmca_coll_demo.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_demo_la_SOURCES = $(sources)
mca_coll_demo_la_LIBADD = $(LIBMPI_LA)
mca_coll_demo_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_demo_la_SOURCES = $(sources)
libmca_coll_demo_la_LIBADD = src/libmca_coll_demo.la
libmca_coll_demo_la_LDFLAGS = -module -avoid-version

# Source files

sources = \
        coll_demo.h \
        coll_demo.c \
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
        coll_demo_reduce.c \
        coll_demo_reduce_scatter.c \
        coll_demo_scan.c \
        coll_demo_exscan.c \
        coll_demo_scatter.c \
        coll_demo_scatterv.c
