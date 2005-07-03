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
AM_CPPFLAGS = -I$(top_srcdir)/test/support
AM_LDFLAGS = -lpthread

check_PROGRAMS = \
	opal_thread \
	opal_condition

TESTS = \
	$(check_PROGRAMS)

opal_thread_SOURCES = opal_thread.c
opal_thread_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la
opal_thread_DEPENDENCIES = $(opal_thread_LDADD)

opal_condition_SOURCES = opal_condition.c
opal_condition_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la
opal_condition_DEPENDENCIES = $(opal_condition_LDADD)

