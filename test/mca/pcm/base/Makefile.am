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

EXTRA_DIST = \
	test1_out_std \
	test2_out_std 

noinst_PROGRAMS = \
	build_env \
	sched_comm

build_env_SOURCES = \
	build_env.c
build_env_LDADD = \
	$(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
build_env_DEPENDENCIES = $(build_env_LDADD)

sched_comm_SOURCES = \
	sched_comm.c
sched_comm_LDADD = \
	$(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
sched_comm_DEPENDENCIES = $(sched_comm_LDADD)

