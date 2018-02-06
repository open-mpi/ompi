# Copyright © 2009-2016 Inria.  All rights reserved.
# Copyright © 2009-2013, 2017 Université Bordeaux
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.

#
# Make sure that the documentation examples work
#

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

LDADD = $(HWLOC_top_builddir)/hwloc/libhwloc.la
DEPENDENCIES = $(LDADD)

TESTS = hwloc-hello
if HWLOC_HAVE_CXX
TESTS += hwloc-hello-cpp
endif HWLOC_HAVE_CXX

check_PROGRAMS = $(TESTS) cpuset+bitmap+cpubind nodeset+membind+policy sharedcaches get-knl-modes gpu

hwloc-hello-cpp.cpp: $(srcdir)/hwloc-hello.c
	cp -f $(srcdir)/hwloc-hello.c $@

clean-local:
	rm -f hwloc-hello-cpp.cpp

hwloc_hello_cpp_SOURCES = hwloc-hello-cpp.cpp
