# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# These are singleton MPI tool / MPI_T tests: they need no launcher
# (an MPI_T-only test runs without MPI_Init; a test that does call
# MPI_Init runs as a singleton), so they can run as part of 'make check'.
if PROJECT_OMPI
    check_PROGRAMS = mpi_t_category_get_events mpi_t_errcodes mpi_t_event_smoke \
                     mpi_t_event_producers mpi_t_event_selftest mpi_t_event_inert_ok \
                     mpi_t_event_reinit mpi_t_event_14019 mpi_t_event_world \
                     mpi_t_event_comm_name
    TESTS = $(check_PROGRAMS)

    LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
    AM_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)

    mpi_t_category_get_events_SOURCES = mpi_t_category_get_events.c
    mpi_t_category_get_events_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    mpi_t_category_get_events_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    mpi_t_errcodes_SOURCES = mpi_t_errcodes.c
    mpi_t_errcodes_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    mpi_t_errcodes_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    mpi_t_event_smoke_SOURCES = mpi_t_event_smoke.c
    mpi_t_event_smoke_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    mpi_t_event_smoke_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

    mpi_t_event_producers_SOURCES = mpi_t_event_producers.c
    mpi_t_event_producers_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    mpi_t_event_producers_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la
endif # PROJECT_OMPI

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo *.log *.o *.trs Makefile \
		mpi_t_category_get_events mpi_t_errcodes mpi_t_event_smoke \
		mpi_t_event_producers mpi_t_event_selftest mpi_t_event_inert_ok \
		mpi_t_event_reinit mpi_t_event_14019 mpi_t_event_world \
		mpi_t_event_comm_name
