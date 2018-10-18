#
# Copyright (c) 2018 The University of Tennessee and The University
#                    of Tennessee Research Foundation.  All rights
#                    reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This test requires multiple processes to run. Don't run it as part
# of 'make check'
if PROJECT_OMPI
    noinst_PROGRAMS = spc_test
    spc_test_SOURCES = spc_test.c
    spc_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
    spc_test_LDADD = \
        $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
        $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
endif # PROJECT_OMPI

distclean:
	rm -rf *.dSYM .deps .libs *.la *.lo spc_test prof *.log *.o *.trs Makefile
