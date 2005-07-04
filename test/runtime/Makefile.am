# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = \
            -I$(top_srcdir)/test/support

check_PROGRAMS = \
	sigchld \
    start_shut

TESTS = \
	$(check_PROGRAMS)

sigchld_SOURCES = \
	sigchld.c
sigchld_LDADD = \
	$(top_builddir)/orte/liborte.la \
	$(top_builddir)/opal/libopal.la \
	$(top_builddir)/test/support/libsupport.a
sigchld_DEPENDENCIES = $(sigchld_LDADD)

start_shut_SOURCES = \
    start_shut.c
start_shut_LDADD = \
	$(top_builddir)/orte/liborte.la \
	$(top_builddir)/opal/libopal.la \
	$(top_builddir)/test/support/libsupport.a
start_shut_DEPENDENCIES = $(start_shut_LDADD)
