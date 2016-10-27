#
# Copyright (c) 2013-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2016 Inria.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
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
    noinst_PROGRAMS = monitoring_test test_pvar_access test_overhead
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
    test_overhead_SCRIPTS = test_overhead.sh
    test_overheaddir = $(bindir)/test/monitoring

    CLEANFILES += $(test_overhead_SCRIPTS)

install-exec-hook:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(top_builddir)/test/monitoring; rm -f $(test_overhead_SCRIPTS); $(LN_S)  $(top_srcdir)/test/monitoring/test_overhead.sh test_overhead.sh)
	(rm -f $(DESTDIR)$(bindir)/$(test_overhead_SCRIPTS); cp -u $(top_srcdir)/test/monitoring/test_overhead.sh $(DESTDIR)$(bindir)/test_overhead.sh)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/test_overhead.sh


if MCA_BUILD_ompi_pml_monitoring_DSO
    lib_LTLIBRARIES = ompi_monitoring_prof.la
    ompi_monitoring_prof_la_SOURCES = monitoring_prof.c
    ompi_monitoring_prof_la_LDFLAGS= \
        -module -avoid-version -shared $(WRAPPER_EXTRA_LDFLAGS)
    ompi_monitoring_prof_la_LIBADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
endif # MCA_BUILD_ompi_pml_monitoring_DSO

endif # PROJECT_OMPI

distclean:
	rm -rf *.dSYM .deps .libs *.la *.lo monitoring_test test_overhead{,.sh} prof *.log *.o *.trs Makefile
