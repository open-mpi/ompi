# -*- makefile -*-
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

TESTS = \
        test_symbol_resolution

check_PROGRAMS = \
        $(TESTS)

# Test for symbol resolution between libmpi and libopen_mpi
TESTS_ENVIRONMENT = \
        OMPI_REEXPORT_TEST_LIBMPI="$(top_builddir)/ompi/.libs/lib@OMPI_LIBMPI_NAME@.@OPAL_DYN_LIB_SUFFIX@"

test_symbol_resolution_SOURCES = test_symbol_resolution.c
test_symbol_resolution_LDADD = $(top_builddir)/ompi/libmpi.la
test_symbol_resolution_LDFLAGS = -ldl