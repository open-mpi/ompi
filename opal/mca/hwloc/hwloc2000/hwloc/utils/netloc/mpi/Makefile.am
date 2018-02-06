# Copyright © 2016-2018 Inria.  All rights reserved.
#
# See COPYING in top-level directory.
#
# $HEADER$
#

AM_CPPFLAGS = $(HWLOC_CPPFLAGS) \
        -I$(top_builddir)/include \
        -I$(top_srcdir)/include
AM_LDFLAGS = $(HWLOC_LDFLAGS)

LDADD = $(top_builddir)/netloc/libnetloc.la \
		$(top_builddir)/hwloc/libhwloc.la

if BUILD_MPITOOLS
bin_PROGRAMS = \
        netloc_rank_order \
        netloc_mpi_find_hosts

if BUILD_NETLOCSCOTCH
bin_PROGRAMS += \
        netloc_mpi_rank_file
endif
endif

netloc_rank_order_SOURCES = netloc_rank_order.c

netloc_mpi_find_hosts_SOURCES = netloc_mpi_find_hosts.c
netloc_mpi_find_hosts_CPPFLAGS = $(AM_CPPFLAGS) $(MPI_CPPFLAGS)
netloc_mpi_find_hosts_LDADD = $(LDADD) $(MPI_LDADD)

if BUILD_NETLOCSCOTCH
netloc_mpi_rank_file_SOURCES = netloc_mpi_rank_file.c
netloc_mpi_rank_file_LDADD = $(LDADD) \
        $(top_builddir)/netloc/libnetlocscotch.la \
        -lscotch -lscotcherr -lm -lpthread
endif
