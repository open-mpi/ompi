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

check_PROGRAMS = \
	ns_replica \
	ns_proxy \
	ns_nds

TESTS = \
	$(check_PROGRAMS)

ns_replica_SOURCES = ns_replica.c
ns_replica_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la 
ns_replica_DEPENDENCIES = $(ns_replica_LDADD)

ns_proxy_SOURCES = ns_proxy.c
ns_proxy_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la 
ns_proxy_DEPENDENCIES = $(ns_proxy_LDADD)

ns_nds_SOURCES = ns_nds.c
ns_nds_LDADD = \
        $(top_builddir)/test/support/libsupport.a \
        $(top_builddir)/src/libmpi.la 
ns_nds_DEPENDENCIES = $(ns_nds_LDADD)
