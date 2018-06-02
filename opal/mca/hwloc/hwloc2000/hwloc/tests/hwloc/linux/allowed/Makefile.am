# Copyright © 2009-2017 Inria.  All rights reserved.
# See COPYING in top-level directory.

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

tests_outputs = \
	test1.output

tests_synthetics = \
	test1.synthetic

tests_fsroots = \
	test1.fsroot.tar.bz2

tests_options = \
	test1.options

tests_envs =

if HWLOC_HAVE_OPENAT
if HWLOC_HAVE_BUNZIPP
TESTS = $(tests_outputs)
endif HWLOC_HAVE_BUNZIPP
endif HWLOC_HAVE_OPENAT

EXTRA_DIST = $(tests_outputs) $(tests_synthetics) $(tests_fsroots) $(tests_options) $(tests_envs)

LOG_COMPILER = $(builddir)/test-topology.sh
