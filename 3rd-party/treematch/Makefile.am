#
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = tm_bucket.h \
    tm_mapping.h \
    tm_timings.h tm_tree.h \
    tm_kpartitioning.h uthash.h\
    IntConstantInitializedVector.h \
    tm_mt.h fibo.h \
    tm_thread_pool.h tm_verbose.h \
    tm_malloc.h k-partitioning.h\
    tm_solution.h tm_topology.h\
    PriorityQueue.h \
    IntConstantInitializedVector.c \
    tm_mt.c fibo.c \
    tm_thread_pool.c tm_verbose.c \
    tm_malloc.c treematch.h \
    tm_mapping.c tm_timings.c \
    tm_bucket.c tm_tree.c \
    tm_topology.c tm_kpartitioning.c \
    tm_solution.c  k-partitioning.c \
    PriorityQueue.c
EXTRA_DIST = COPYING LICENSE

noinst_LTLIBRARIES = libtreematch.la
libtreematch_la_SOURCES = $(sources)
libtreematch_la_LDFLAGS =

distclean-local:
	rm config.h
