# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This is a single-process partitioned communication test.  It calls
# MPI_Init and performs all of its communication on MPI_COMM_SELF, so it
# needs no launcher and can run as part of 'make check'.  It exercises the
# MPI_Parrived null / inactive request behavior required by MPI-5.0
# sec. 4.2.2 (Open MPI issue #14003).

# Make "make check" work in an uninstalled --enable-mca-dso build tree
include $(top_srcdir)/Makefile.mca-dso-check

check_PROGRAMS = parrived_null_inactive
TESTS = $(check_PROGRAMS)

common_ldflags = $(OMPI_PKG_CONFIG_LDFLAGS)
common_ldadd = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

parrived_null_inactive_SOURCES = parrived_null_inactive.c
parrived_null_inactive_LDFLAGS = $(common_ldflags)
parrived_null_inactive_LDADD = $(common_ldadd)

distclean-local:
	rm -rf *.dSYM .deps .libs *.la *.lo *.log *.o *.trs \
		parrived_null_inactive Makefile
