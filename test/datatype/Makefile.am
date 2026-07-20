#
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2014-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# OPAL-level datatype tests: these link only against libopen-pal.  The
# tests of the OMPI datatype engine and the MPI datatype API live in
# ompi/test/datatype.
#

AM_CPPFLAGS = -I$(top_srcdir)/test/support

# These tests call opal_init().  Make "make check" work in an
# uninstalled --enable-mca-dso build tree.
include $(top_srcdir)/Makefile.mca-dso-check

TESTS = opal_datatype_test opal_datatype_bigcount unpack_hetero opal_ddt_api

check_PROGRAMS = $(TESTS)

opal_datatype_test_SOURCES = opal_datatype_test.c opal_ddt_lib.c opal_ddt_lib.h
opal_datatype_test_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
opal_datatype_test_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

opal_datatype_bigcount_SOURCES = opal_datatype_bigcount.c
opal_datatype_bigcount_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
opal_datatype_bigcount_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

unpack_hetero_SOURCES = unpack_hetero.c
unpack_hetero_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
unpack_hetero_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la

opal_ddt_api_SOURCES = opal_ddt_api.c
opal_ddt_api_LDFLAGS = $(OMPI_PKG_CONFIG_LDFLAGS)
opal_ddt_api_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.o *.trs $(check_PROGRAMS) Makefile
