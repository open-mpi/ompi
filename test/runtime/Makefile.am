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
	parse_context

TESTS = \
	$(check_PROGRAMS)

parse_context_SOURCES = \
	parse_context.c
parse_context_LDADD = \
	$(top_builddir)/src/libmpi.la \
	$(top_builddir)/test/support/libsupport.a

parse_context_LDFLAGS = $(LIBMPI_EXTRA_LDFLAGS)
parse_context_DEPENDENCIES = $(parse_context_LDADD)


sigchld_SOURCES = \
	sigchld.c
sigchld_LDADD = \
	$(top_builddir)/src/libmpi.la \
	$(top_builddir)/test/support/libsupport.a
sigchld_DEPENDENCIES = $(sigchld_LDADD)
