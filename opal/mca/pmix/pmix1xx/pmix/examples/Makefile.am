#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = -I$(prefix) -I$(prefix)/include

bin_PROGRAMS = server client pub dmodex fault dynamic

server_SOURCES = \
        server.c

server_LDADD = \
	$(top_builddir)/libpmix.la

client_SOURCES = \
        client.c

client_LDADD = \
	$(top_builddir)/libpmix.la

pub_SOURCES = \
        pub.c

pub_LDADD = \
	$(top_builddir)/libpmix.la

dmodex_SOURCES = \
        dmodex.c

dmodex_LDADD = \
	$(top_builddir)/libpmix.la

fault_SOURCES = \
        fault.c

fault_LDADD = \
	$(top_builddir)/libpmix.la

dynamic_SOURCES = \
        dynamic.c

dynamic_LDADD = \
	$(top_builddir)/libpmix.la
