#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

AM_CPPFLAGS = -I$(srcdir)/libevent -I$(srcdir)/libevent/include -I$(builddir)/libevent/include

SUBDIRS = libevent

headers = libevent207.h

sources = \
        libevent207_component.c \
        libevent207_module.c

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(includedir)/openmpi/$(subdir)
nobase_opal_HEADERS = $(headers)
endif

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_event_libevent207_DSO
component_noinst =
component_install = mca_event_libevent207.la
else
component_noinst = libmca_event_libevent207.la
component_install =
endif

# We only ever build this component statically

noinst_LTLIBRARIES = $(component_noinst)
libmca_event_libevent207_la_SOURCES =$(sources)
libmca_event_libevent207_la_LDFLAGS = -module -avoid-version
libmca_event_libevent207_la_LIBADD = $(builddir)/libevent/libevent.la
