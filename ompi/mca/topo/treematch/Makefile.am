#
# Copyright (c) 2011-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2011-2015 INRIA.  All rights reserved.
# Copyright (c) 2011-2015 Université Bordeaux 1
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if topo_treematch_local
extra_treematch_files = treematch/tm_bucket.h \
    treematch/tm_hwloc.h treematch/tm_mapping.h \
    treematch/tm_timings.h treematch/tm_tree.h \
    treematch/tm_kpartitioning.h treematch/uthash.h\
    treematch/fibo.h treematch/IntConstantInitializedVector.h \
    treematch/k-partitioning.h treematch/tm_mt.h \
    treematch/tm_thread_pool.h treematch/tm_verbose.h \
    treematch/PriorityQueue.h treematch/tm_malloc.h \
    treematch/fibo.c treematch/IntConstantInitializedVector.c \
    treematch/k-partitioning.c treematch/tm_mt.c \
    treematch/tm_thread_pool.c treematch/tm_verbose.c \
    treematch/PriorityQueue.c treematch/tm_malloc.c \
    treematch/tm_mapping.c treematch/tm_timings.c \
    treematch/tm_bucket.c treematch/tm_tree.c \
    treematch/tm_hwloc.c treematch/tm_kpartitioning.c
endif

sources = \
    topo_treematch.h \
    topo_treematch_module.c \
    topo_treematch_component.c \
    topo_treematch_dist_graph_create.c $(extra_treematch_files)

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_topo_treematch_DSO
lib =
lib_sources =
component = mca_topo_treematch.la
component_sources = $(sources)
else
lib = libmca_topo_treematch.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_topo_treematch_la_SOURCES = $(component_sources)
mca_topo_treematch_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_topo_treematch_la_SOURCES = $(lib_sources)
libmca_topo_treematch_la_LDFLAGS = -module -avoid-version

