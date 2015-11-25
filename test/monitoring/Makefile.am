#
# Copyright (c) 2013-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2015 Inria.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This test requires multiple processes to run. Don't run it as part
# of 'make check'
if PROJECT_OMPI
    noinst_PROGRAMS = monitoring_test
    monitoring_test_SOURCES = monitoring_test.c
    monitoring_test_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
    monitoring_test_LDADD = $(top_builddir)/ompi/libmpi.la $(top_builddir)/opal/libopen-pal.la

if MCA_BUILD_ompi_pml_monitoring_DSO
    lib_LTLIBRARIES = monitoring_prof.la
    monitoring_prof_la_SOURCES = monitoring_prof.c
    monitoring_prof_la_LDFLAGS=-module -avoid-version -shared $(WRAPPER_EXTRA_LDFLAGS)
    monitoring_prof_la_LIBADD = $(top_builddir)/ompi/libmpi.la $(top_builddir)/opal/libopen-pal.la
endif

endif

distclean:
	rm -rf *.dSYM .deps .libs *.la *.lo monitoring_test *.log *.o *.trs Makefile
