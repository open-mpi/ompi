# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	test_ns_replica 

test_ns_replica_SOURCES = test_ns_replica.c
test_ns_replica_LDADD = \
        $(top_builddir)/src/class/ompi_object.lo \
        $(top_builddir)/src/util/malloc.lo \
        $(top_builddir)/src/util/output.lo \
        $(top_builddir)/src/util/argv.lo \
        $(top_builddir)/src/util/strncpy.lo \
        $(top_builddir)/src/mca/ns/base/libmca_ns_base.la \
        $(top_builddir)/src/mca/ns/replica/src/libmca_ns_replica.la \
        $(top_builddir)/src/mca/ns/proxy/src/libmca_ns_proxy.la \
        $(top_builddir)/src/libmpi.la \
        $(top_builddir)/src/threads/mutex.lo \
        $(top_builddir)/src/threads/mutex_pthread.lo \
        $(top_builddir)/src/threads/mutex_spinlock.lo \
        $(top_builddir)/src/threads/mutex_spinwait.lo \
        $(top_builddir)/test/support/libsupport.la 
test_ns_replica_DEPENDENCIES = $(test_ns_replica_LDADD)
