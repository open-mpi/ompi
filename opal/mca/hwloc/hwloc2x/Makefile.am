#
# Copyright (c) 2011-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2015 Intel, Inc. All right reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = autogen.subdirs

SUBDIRS = hwloc
DIST_SUBDIRS=hwloc

# Headers and sources
headers = hwloc2x.h
sources = hwloc2x_component.c

libs = hwloc/hwloc/libhwloc_embedded.la

# We only ever build this component statically
noinst_LTLIBRARIES = libmca_hwloc_hwloc2x.la
libmca_hwloc_hwloc2x_la_SOURCES = $(headers) $(sources)
nodist_libmca_hwloc_hwloc2x_la_SOURCES = $(nodist_headers)
libmca_hwloc_hwloc2x_la_LDFLAGS = -module -avoid-version
libmca_hwloc_hwloc2x_la_LIBADD = $(libs)
libmca_hwloc_hwloc2x_la_DEPENDENCIES = $(libs)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
nobase_nodist_opal_HEADERS = $(nodist_headers)
endif
