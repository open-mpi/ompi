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
check_PROGRAMS = mpi_t_category_get_events mpi_t_errcodes mpi_t_event_smoke \
                 mpi_t_event_producers mpi_t_event_selftest mpi_t_event_inert_ok \
                 mpi_t_event_reinit mpi_t_event_14019 mpi_t_event_world \
                 mpi_t_event_comm_name
TESTS = $(check_PROGRAMS)

# Every test here is a single .c file named after the test, so Automake's
# default _SOURCES is what we want, and they all link the same way.
AM_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo *.log *.o *.trs Makefile \
		$(check_PROGRAMS)
