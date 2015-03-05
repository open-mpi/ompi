#
# Copyright (c) 2013-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2015 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if PROJECT_OMPI
    TESTS = monitoring_test
endif

check_PROGRAMS = $(TESTS)

monitoring_test_SOURCES = monitoring_test.c
monitoring_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
monitoring_test_LDADD = $(top_builddir)/ompi/libmpi.la $(top_builddir)/opal/libopen-pal.la

