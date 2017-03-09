#
# Copyright (c) 2013-2016 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2016 Inria.  All rights reserved.
# Copyright (c) 2015-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
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
    monitoring_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
    monitoring_test_LDADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(top_builddir)/opal/libopen-pal.la
    test_pvar_access_SOURCES = test_pvar_access.c
    test_pvar_access_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
    test_pvar_access_LDADD = $(top_builddir)/ompi/libmpi.la $(top_builddir)/opal/libopen-pal.la
    test_overhead_SOURCES = test_overhead.c
    test_overhead_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
    test_overhead_LDADD = $(top_builddir)/ompi/libmpi.la $(top_builddir)/opal/libopen-pal.la
    check_monitoring_SOURCES = check_monitoring.c
    check_monitoring_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
    check_monitoring_LDADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(top_builddir)/opal/libopen-pal.la
    example_reduce_count_SOURCES = example_reduce_count.c
    example_reduce_count_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
    example_reduce_count_LDADD = $(top_builddir)/ompi/libmpi.la $(top_builddir)/opal/libopen-pal.la

    test_overheaddir = $(bindir)/test/monitoring

    CLEANFILES += $(test_overhead_SCRIPTS)

if MCA_BUILD_ompi_pml_monitoring_DSO
    lib_LTLIBRARIES = monitoring_prof.la
    monitoring_prof_la_SOURCES = monitoring_prof.c
    monitoring_prof_la_LDFLAGS=-module -avoid-version -shared $(WRAPPER_EXTRA_LDFLAGS)
    monitoring_prof_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la $(top_builddir)/opal/libopen-pal.la
endif

endif

distclean:
	rm -rf *.dSYM .deps .libs *.la *.lo monitoring_test test_pvar_access test_overhead check_monitoring example_reduce_count prof *.log *.o *.trs Makefile
