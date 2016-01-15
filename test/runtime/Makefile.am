# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#



check_PROGRAMS = \
	opal_init_finalize \
	orte_init_finalize \
	sigchld \
	start_shut

TESTS = \
	$(check_PROGRAMS)

sigchld_SOURCES = \
	sigchld.c
sigchld_LDADD = \
	$(top_builddir)/orte/libopen-rte.la \
	$(top_builddir)/test/support/libsupport.a
sigchld_DEPENDENCIES = $(sigchld_LDADD)

start_shut_SOURCES = \
    start_shut.c
start_shut_LDADD = \
	$(top_builddir)/orte/libopen-rte.la \
	$(top_builddir)/test/support/libsupport.a
start_shut_DEPENDENCIES = $(start_shut_LDADD)

orte_init_finalize_SOURCES = \
    orte_init_finalize.c
orte_init_finalize_LDADD = \
	$(top_builddir)/orte/libopen-rte.la \
orte_init_finalize_DEPENDENCIES = $(orte_init_finalize_LDADD)

opal_init_finalize_SOURCES = \
    opal_init_finalize.c
opal_init_finalize_LDADD = \
	$(top_builddir)/opal/libopen-pal.la \
	$(top_builddir)/test/support/libsupport.a
opal_init_finalize_DEPENDENCIES = $(opal_init_finalize_LDADD)

distclean:
	rm -rf *.dSYM .deps *.log *.o *.trs $(check_PROGRAMS) Makefile
