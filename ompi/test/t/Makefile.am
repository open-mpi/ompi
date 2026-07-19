# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# These are singleton MPI tool / MPI_T tests: they need no launcher (an
# MPI_T-only test runs without MPI_Init; a test that calls MPI_Init or
# MPI_Session_init runs as a singleton), so they can run as part of
# 'make check'.
#
# The mpi_t_lifecycle.h family walks the MPI_T init/finalize orderings
# against the world model and the sessions model.  Sessions repeat, so
# every sessions interleaving lives in one program; MPI_Init() is legal
# only once per process, so each world-model ordering is its own program.
check_PROGRAMS = \
        mpi_t_category_get_events \
        mpi_t_concurrent_init \
        mpi_t_level_no_downgrade \
        mpi_t_level_pinning \
        mpi_t_lifecycle \
        mpi_t_outlives_mpi \
        mpi_t_repeated_sessions \
        mpi_t_world_close_first \
        mpi_t_world_inner \
        mpi_t_world_outlast \
        mpi_t_sessions \
        mpi_t_stress \
        mpi_t_world_sessions
TESTS = $(check_PROGRAMS)

test_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

mpi_t_category_get_events_SOURCES = mpi_t_category_get_events.c
mpi_t_category_get_events_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_category_get_events_LDADD = $(test_ldadd)

mpi_t_concurrent_init_SOURCES = mpi_t_concurrent_init.c mpi_t_lifecycle.h
mpi_t_concurrent_init_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
# This test calls pthread_* APIs directly, so it explicitly links
# -lpthread -- which has been working on all supported systems since
# 42a05e9a741 (2004) established the same convention in
# test/threads/Makefile.am.  Configure's THREAD_CFLAGS / THREAD_LDFLAGS
# / THREAD_LIBS describe whatever threading package the threads MCA
# framework selected (not necessarily pthreads), and are only applied
# to the wrapper compilers -- they are not AC_SUBSTed for Makefile use.
mpi_t_concurrent_init_LDADD = $(test_ldadd) -lpthread

mpi_t_level_no_downgrade_SOURCES = mpi_t_level_no_downgrade.c mpi_t_lifecycle.h
mpi_t_level_no_downgrade_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_level_no_downgrade_LDADD = $(test_ldadd)

mpi_t_level_pinning_SOURCES = mpi_t_level_pinning.c mpi_t_lifecycle.h
mpi_t_level_pinning_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_level_pinning_LDADD = $(test_ldadd)

mpi_t_lifecycle_SOURCES = mpi_t_lifecycle.c mpi_t_lifecycle.h
mpi_t_lifecycle_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_lifecycle_LDADD = $(test_ldadd)

mpi_t_outlives_mpi_SOURCES = mpi_t_outlives_mpi.c
mpi_t_outlives_mpi_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_outlives_mpi_LDADD = $(test_ldadd)

mpi_t_world_close_first_SOURCES = mpi_t_world_close_first.c mpi_t_lifecycle.h
mpi_t_world_close_first_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_world_close_first_LDADD = $(test_ldadd)

mpi_t_world_inner_SOURCES = mpi_t_world_inner.c mpi_t_lifecycle.h
mpi_t_world_inner_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_world_inner_LDADD = $(test_ldadd)

mpi_t_world_outlast_SOURCES = mpi_t_world_outlast.c mpi_t_lifecycle.h
mpi_t_world_outlast_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_world_outlast_LDADD = $(test_ldadd)

mpi_t_repeated_sessions_SOURCES = mpi_t_repeated_sessions.c mpi_t_lifecycle.h
mpi_t_repeated_sessions_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_repeated_sessions_LDADD = $(test_ldadd)

mpi_t_sessions_SOURCES = mpi_t_sessions.c mpi_t_lifecycle.h
mpi_t_sessions_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_sessions_LDADD = $(test_ldadd)

mpi_t_stress_SOURCES = mpi_t_stress.c mpi_t_lifecycle.h
mpi_t_stress_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_stress_LDADD = $(test_ldadd)

mpi_t_world_sessions_SOURCES = mpi_t_world_sessions.c mpi_t_lifecycle.h
mpi_t_world_sessions_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_world_sessions_LDADD = $(test_ldadd)

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo $(check_PROGRAMS) *.log *.o *.trs Makefile
