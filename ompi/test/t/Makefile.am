# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# These are singleton MPI_T tests: they exercise the MPI tool
# information interface without MPI_Init, so they need no launcher and
# can run as part of 'make check'.
check_PROGRAMS = mpi_t_category_get_events
TESTS = $(check_PROGRAMS)

mpi_t_category_get_events_SOURCES = mpi_t_category_get_events.c
mpi_t_category_get_events_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
mpi_t_category_get_events_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo mpi_t_category_get_events *.log *.o *.trs Makefile
