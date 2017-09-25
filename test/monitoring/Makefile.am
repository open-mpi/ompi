#
# Copyright (c) 2013-2017 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2017 Inria.  All rights reserved.
# Copyright (c) 2015-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This test requires multiple processes to run. Don't run it as part
# of 'make check'
if PROJECT_OMPI
    noinst_PROGRAMS = monitoring_test test_pvar_access test_overhead check_monitoring example_reduce_count
    monitoring_test_SOURCES = monitoring_test.c
    monitoring_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    monitoring_test_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
    test_pvar_access_SOURCES = test_pvar_access.c
    test_pvar_access_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    test_pvar_access_LDADD = \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
    test_overhead_SOURCES = test_overhead.c
    test_overhead_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    test_overhead_LDADD = \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
    check_monitoring_SOURCES = check_monitoring.c
    check_monitoring_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    check_monitoring_LDADD = \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
    example_reduce_count_SOURCES = example_reduce_count.c
    example_reduce_count_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    example_reduce_count_LDADD = \
	$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
	$(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
endif # PROJECT_OMPI

distclean:
	rm -rf *.dSYM .deps .libs *.la *.lo monitoring_test test_pvar_access test_overhead check_monitoring example_reduce_count prof *.log *.o *.trs Makefile
