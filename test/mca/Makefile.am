# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2007 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2024      Google, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS="-I$(top_srcdir)/test/support"

check_PROGRAMS = mca_var

TESTS = $(check_PROGRAMS)

mca_var_SOURCES = mca_var.c
mca_var_LDADD = \
        $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
        $(top_builddir)/test/support/libsupport.a
mca_var_DEPENDENCIES = $(mca_var_LDADD)

clean-local:
	rm -f mca_var

distclean-local:
	rm -rf *.dSYM .deps .libs *.log *.txt *.o *.trs $(check_PROGRAMS) Makefile
