# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1
AM_LDFLAGS = -lpthread

check_PROGRAMS = \
	ompi_thread \
	ompi_condition

TESTS = \
	$(check_PROGRAMS)

ompi_thread_SOURCES = ompi_thread.c
ompi_thread_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la
ompi_thread_DEPENDENCIES = $(ompi_thread_LDADD)

ompi_condition_SOURCES = ompi_condition.c
ompi_condition_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la
ompi_condition_DEPENDENCIES = $(ompi_condition_LDADD)

