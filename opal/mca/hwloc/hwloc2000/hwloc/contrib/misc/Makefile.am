# Copyright © 2009-2017 Inria.  All rights reserved.
#
# See COPYING in top-level directory.

# This makefile is only reached when building in standalone mode

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

LDADD = $(HWLOC_top_builddir)/hwloc/libhwloc.la

check_PROGRAMS = hwloc-tweak-osindex

EXTRA_DIST = hwloc-export-sysfs
