# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_lam_builddir)/src/include \
        -I$(top_lam_srcdir)/src \
        -I$(top_lam_srcdir)/src/include

noinst_LTLIBRARIES = libmca_coll_basic.la
libmca_coll_basic_la_SOURCES = \
        coll_basic.h \
        coll_basic.c \
        coll_basic_allgather.c \
        coll_basic_allgatherv.c \
        coll_basic_allreduce.c \
        coll_basic_alltoall.c \
        coll_basic_alltoallv.c \
        coll_basic_alltoallw.c \
        coll_basic_barrier.c \
        coll_basic_bcast.c \
        coll_basic_gather.c \
        coll_basic_gatherv.c \
        coll_basic_module.c \
        coll_basic_reduce.c \
        coll_basic_reduce_scatter.c \
        coll_basic_scan.c \
        coll_basic_exscan.c \
        coll_basic_scatter.c \
        coll_basic_scatterv.c
