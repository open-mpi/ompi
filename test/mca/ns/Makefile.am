# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
AM_CPPFLAGS = -I$(top_srcdir)/test/support -DOMPI_ENABLE_DEBUG_OVERRIDE=1

noinst_PROGRAMS = \
	test_ns_replica \
	test_ns_proxy

test_ns_replica_SOURCES = test_ns_replica.c
test_ns_replica_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la 
test_ns_replica_DEPENDENCIES = $(test_ns_replica_LDADD)

test_ns_proxy_SOURCES = test_ns_proxy.c
test_ns_proxy_LDADD = \
        $(top_builddir)/test/support/libsupport.la \
        $(top_builddir)/src/libmpi.la 
test_ns_proxy_DEPENDENCIES = $(test_ns_proxy_LDADD)
