# Copyright © 2014 Cisco Systems, Inc.  All rights reserved.
# Copyright © 2014      University of Wisconsin-La Crosse.
#                         All rights reserved.
#
# Copyright © 2016-2018 Inria.  All rights reserved.
# See COPYING in top-level directory.
#
# $HEADER$
#

AM_CPPFLAGS = \
        $(NETLOC_CPPFLAGS) \
        $(HWLOC_CPPFLAGS)

# If we're in standalone mode, build the installable library.
# Otherwise, build the embedded library.

if HWLOC_BUILD_STANDALONE
lib_LTLIBRARIES = libnetloc.la
else
noinst_LTLIBRARIES = libnetloc_embedded.la
endif

sources = \
        support.c \
        topology.c \
        edge.c \
        node.c \
        physical_link.c \
        path.c \
        architecture.c \
        hwloc.c \
        mpicomm.c


# Installable library

libnetloc_la_SOURCES = $(sources)
libnetloc_la_LDFLAGS = $(ldflags) -version-info $(libnetloc_so_version)
libnetloc_la_LIBADD = \
        $(top_builddir)/hwloc/libhwloc.la

# Embedded library (note the lack of a .so version number -- that
# intentionally only appears in the installable library).  Also note
# the lack of _LDFLAGS -- all libs are added by the upper layer (via
# HWLOC_EMBEDDED_LIBS).

libnetloc_embedded_la_SOURCES = $(sources)
libnetloc_embedded_la_LDFLAGS = $(ldflags)
libnetloc_embedded_la_LIBADD = \
        $(HWLOC_top_builddir)/hwloc/libhwloc_embedded.la

scotchsources = \
        scotch.c

if BUILD_NETLOCSCOTCH
# If we're in standalone mode, build the installable library.
# Otherwise, build the embedded library.

if HWLOC_BUILD_STANDALONE
lib_LTLIBRARIES += libnetlocscotch.la
else
noinst_LTLIBRARIES += libnetlocscotch_embedded.la
endif

# Installable library

libnetlocscotch_la_SOURCES = $(scotchsources)
libnetlocscotch_la_LDFLAGS = -version-info $(libnetloc_so_version)
libnetlocscotch_la_LIBADD = \
        $(top_builddir)/hwloc/libhwloc.la \
        libnetloc.la

# Embedded library (note the lack of a .so version number -- that
# intentionally only appears in the installable library).  Also note
# the lack of _LDFLAGS -- all libs are added by the upper layer (via
# HWLOC_EMBEDDED_LIBS).

libnetlocscotch_embedded_la_SOURCES = $(scotchsources)
libnetlocscotch_embedded_la_LDFLAGS =
libnetlocscotch_embedded_la_LIBADD = \
        $(HWLOC_top_builddir)/hwloc/libhwloc_embedded.la \
        $(HWLOC_top_builddir)/hwloc/libnetloc_embedded.la

else !BUILD_NETLOCSCOTCH
EXTRA_DIST = $(scotchsources)
endif !BUILD_NETLOCSCOTCH
