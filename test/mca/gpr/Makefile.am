# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	test_gpr_replica 

test_gpr_replica_SOURCES = test_gpr_replica.c
test_gpr_replica_LDADD = \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/mca/gpr/base/libmca_gpr_base.la \
        $(top_builddir)/src/mca/gpr/replica/libmca_gpr_replica.la \
        $(top_builddir)/src/mca/gpr/proxy/libmca_gpr_proxy.la \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/test/support/libsupport.la 
test_gpr_replica_DEPENDENCIES = $(test_gpr_replica_LDADD)
