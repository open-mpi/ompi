# Copyright 2009 INRIA, Université Bordeaux 1
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

SUBDIRS = linux ports xml

LIBS =

if HWLOC_BUILD_TESTS
check_PROGRAMS = hwloc_cpuset_string \
	hwloc_get_closest_objs \
	hwloc_get_obj_covering_cpuset \
	hwloc_get_cache_covering_cpuset \
	hwloc_get_largest_objs_inside_cpuset \
	hwloc_get_next_obj_covering_cpuset \
	hwloc_get_obj_inside_cpuset \
	hwloc_get_shared_cache_covering_obj \
	hwloc_get_obj_below_array_by_type \
	hwloc_cpuset_first_last_weight \
	hwloc_cpuset_singlify \
	hwloc_type_depth \
	hwloc_bind \
	hwloc_object_userdata \
	hwloc_synthetic \
	hwloc_is_thissystem \
	hwloc_insert_misc

if HWLOC_HAVE_LINUX_LIBNUMA
check_PROGRAMS += linux-libnuma
endif HWLOC_HAVE_LINUX_LIBNUMA

if HWLOC_HAVE_SCHED_SETAFFINITY
check_PROGRAMS += glibc-sched
endif HWLOC_HAVE_SCHED_SETAFFINITY

if HWLOC_HAVE_LIBIBVERBS
check_PROGRAMS += openfabrics-verbs
endif HWLOC_HAVE_LIBIBVERBS

TESTS = $(check_PROGRAMS)

# The library has a different name depending on whether we are
# building in standalone or embedded mode.
if HWLOC_BUILD_STANDALONE
hwloc_lib = libhwloc.la
else
hwloc_lib = libhwloc_embedded.la
endif

LIBS += $(HWLOC_top_builddir)/src/$(hwloc_lib)

linux_libnuma_LDFLAGS = -lnuma
openfabrics_verbs_LDFLAGS = -libverbs
if !HWLOC_HAVE_WINDOWS
hwloc_bind_LDFLAGS = -lpthread
endif !HWLOC_HAVE_WINDOWS

endif HWLOC_BUILD_TESTS
