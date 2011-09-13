#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Need to include these files so that these directories are carried in
# the tarball (in case someone invokes autogen.sh on a dist tarball).
EXTRA_DIST = \
        hwloc/doc/README.txt \
        hwloc/tests/README.txt \
        hwloc/utils/README.txt

SUBDIRS = hwloc

# Headers and sources
headers = hwloc121.h
sources = hwloc121_component.c

# We only ever build this component statically

noinst_LTLIBRARIES = libmca_hwloc_hwloc121.la
libmca_hwloc_hwloc121_la_SOURCES = $(headers) $(sources)
libmca_hwloc_hwloc121_la_LDFLAGS = -module -avoid-version $(opal_hwloc_hwloc121_LDFLAGS)
libmca_hwloc_hwloc121_la_LIBADD = $(opal_hwloc_hwloc121_LIBS)
