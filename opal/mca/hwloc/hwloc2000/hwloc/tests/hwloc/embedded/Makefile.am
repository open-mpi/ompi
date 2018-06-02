# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.

ACLOCAL_AMFLAGS = -I./config -I./hwloc-tree/config

SUBDIRS = hwloc-tree

AM_CPPFLAGS = $(HWLOC_EMBEDDED_CPPFLAGS)

TESTS = main

noinst_PROGRAMS = main
noinst_LTLIBRARIES = libdo_test.la

libdo_test_la_SOURCES = do_test.c

main_SOURCES = main.c
main_LDADD = libdo_test.la $(HWLOC_EMBEDDED_LDADD) $(HWLOC_EMBEDDED_LIBS)
main_DEPENDENCIES = libdo_test.la

EXTRA_DIST = run-embedded-tests.sh
