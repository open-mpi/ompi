# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

EXTRA_DIST = \
	test1_out_std \
	test2_out_std 

noinst_PROGRAMS = \
	sched_comm

sched_comm_SOURCES = \
	sched_comm.c
sched_comm_LDADD = \
	$(top_builddir)/src/libmpi.la \
        $(top_builddir)/test/support/libsupport.la 
sched_comm_DEPENDENCIES = $(sched_comm_LDADD)
