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
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      IBM Corporation. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
headers = examples.h

AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/include -I$(top_builddir)/include -I$(top_builddir)/include/pmix

noinst_PROGRAMS = client client2 dmodex dynamic fault pub pubi \
                  tool debugger debuggerd alloc jctrl group group_dmodex asyncgroup \
                  hello nodeinfo  abi_no_init abi_with_init group_lcl_cid pset log \
                  client3

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

client3_SOURCES = client3.c examples.h
client3_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
client3_LDADD =  $(top_builddir)/src/libpmix.la

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

group_SOURCES = group.c examples.h
group_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
group_LDADD = $(top_builddir)/src/libpmix.la

group_dmodex_SOURCES = group_dmodex.c examples.h
group_dmodex_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
group_dmodex_LDADD = $(top_builddir)/src/libpmix.la

asyncgroup_SOURCES = asyncgroup.c examples.h
asyncgroup_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
asyncgroup_LDADD = $(top_builddir)/src/libpmix.la

if !WANT_HIDDEN
server_SOURCES = server.c examples.h
server_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
server_LDADD = $(top_builddir)/src/libpmix.la
endif

hello_SOURCES = hello.c examples.h
hello_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
hello_LDADD =  $(top_builddir)/src/libpmix.la

abi_no_init_SOURCES = abi_no_init.c
abi_no_init_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
abi_no_init_LDADD =  $(top_builddir)/src/libpmix.la

abi_with_init_SOURCES = abi_with_init.c
abi_with_init_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
abi_with_init_LDADD =  $(top_builddir)/src/libpmix.la

group_lcl_cid_SOURCES = group_lcl_cid.c examples.h
group_lcl_cid_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
group_lcl_cid_LDADD =  $(top_builddir)/src/libpmix.la

nodeinfo_SOURCES = nodeinfo.c examples.h
nodeinfo_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
nodeinfo_LDADD =  $(top_builddir)/src/libpmix.la

pset_SOURCES = pset.c examples.h
pset_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
pset_LDADD = $(top_builddir)/src/libpmix.la

log_SOURCES = log.c examples.h
log_LDFLAGS = $(PMIX_PKG_CONFIG_LDFLAGS)
log_LDADD = $(top_builddir)/src/libpmix.la

distclean-local:
	rm -f *.o alloc asyncgroup bad_exit client client2 \
        debugger debuggerd dmodex dynamic fault group \
        hello jctrl launcher log pub pubi server tool \
        abi_no_init abi_with_init group_lcl_cid pset \
        async_group group_dmodex client3
