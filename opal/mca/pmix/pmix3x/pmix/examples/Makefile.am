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
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
headers = examples.h

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/include -I$(top_builddir)/include/pmix

noinst_PROGRAMS = client client2 dmodex dynamic fault pub pubi tool debugger debuggerd alloc jctrl
if !WANT_HIDDEN
# these examples use internal symbols
# use --disable-visibility
noinst_PROGRAMS += server
endif

client_SOURCES = client.c examples.h
client_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
client_LDADD =  $(top_builddir)/src/libpmix.la

client2_SOURCES = client2.c examples.h
client2_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
client2_LDADD =  $(top_builddir)/src/libpmix.la

debugger_SOURCES = debugger.c examples.h
debugger_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
debugger_LDADD =  $(top_builddir)/src/libpmix.la

debuggerd_SOURCES = debuggerd.c examples.h
debuggerd_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
debuggerd_LDADD =  $(top_builddir)/src/libpmix.la

alloc_SOURCES = alloc.c examples.h
alloc_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
alloc_LDADD =  $(top_builddir)/src/libpmix.la

jctrl_SOURCES = jctrl.c examples.h
jctrl_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
jctrl_LDADD =  $(top_builddir)/src/libpmix.la

dmodex_SOURCES = dmodex.c examples.h
dmodex_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
dmodex_LDADD =  $(top_builddir)/src/libpmix.la

dynamic_SOURCES = dynamic.c examples.h
dynamic_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
dynamic_LDADD = $(top_builddir)/src/libpmix.la

fault_SOURCES = fault.c examples.h
fault_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
fault_LDADD = $(top_builddir)/src/libpmix.la

pub_SOURCES = pub.c examples.h
pub_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pub_LDADD = $(top_builddir)/src/libpmix.la

pubi_SOURCES = pubi.c examples.h
pubi_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pubi_LDADD = $(top_builddir)/src/libpmix.la

tool_SOURCES = tool.c examples.h
tool_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
tool_LDADD = $(top_builddir)/src/libpmix.la

if !WANT_HIDDEN
server_SOURCES = server.c examples.h
server_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
server_LDADD = $(top_builddir)/src/libpmix.la
endif

distclean-local:
	rm -f *.o client debugger debuggerd dmodex dynamic fault pub server
