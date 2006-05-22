#
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

check_PROGRAMS = ddt_test

TESTS = $(check_PROGRAMS)

ddt_test_SOURCES = ddt_test.c
ddt_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
ddt_test_LDADD = $(top_builddir)/ompi/libmpi.la
ddt_test_DEPENDENCIES = $(mpi_peruse_LDADD)

