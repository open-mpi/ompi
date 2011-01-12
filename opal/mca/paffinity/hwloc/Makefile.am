#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS =
if OPAL_PAFFINITY_HWLOC_INTERNAL
SUBDIRS += hwloc
endif

# Need to include these files so that these directories are carried in
# the tarball (in case someone invokes autogen.sh on a dist tarball).
EXTRA_DIST = \
        hwloc/doc/README.txt \
        hwloc/tests/README.txt \
        hwloc/utils/README.txt

# To find hwloc_bottom.h.  EMBEDDED flags are for if we OMPI's
# internal hwloc is used; paffinity_hwloc_CPPFLAGS is if we are using
# an external install.
AM_CPPFLAGS = $(HWLOC_EMBEDDED_CPPFLAGS) $(paffinity_hwloc_CPPFLAGS)
# To get the cflags for the stuff in hwloc.h
AM_CFLAGS = $(HWLOC_EMBEDDED_CFLAGS) $(paffinity_hwloc_CFLAGS)

sources = \
        paffinity_hwloc.h \
        paffinity_hwloc_component.c \
        paffinity_hwloc_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_paffinity_hwloc_DSO
component_noinst =
component_install = mca_paffinity_hwloc.la
else
component_noinst = libmca_paffinity_hwloc.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_paffinity_hwloc_la_SOURCES = $(sources)
mca_paffinity_hwloc_la_LDFLAGS = -module -avoid-version
mca_paffinity_hwloc_la_LIBADD =
if OPAL_PAFFINITY_HWLOC_INTERNAL
# In most circumstances, the .la file should contain all relevant
# LDFLAGS and LIBS that we need.  However, hwloc's embedding takes
# pains to not add anything to the upper-level package LDFLAGS and
# LIBS, so hwloc_embedded.la won't contain what it needs.  Instead, we
# have to add HWLOC_EMBEDDED_LDFLAGS and _LIBS here manually.
mca_paffinity_hwloc_la_LDFLAGS += $(HWLOC_EMBEDDED_LDFLAGS)
mca_paffinity_hwloc_la_LIBADD += \
        $(top_ompi_builddir)/opal/mca/paffinity/hwloc/hwloc/src/libhwloc_embedded.la \
        $(HWLOC_EMBEDDED_LIBS)
else
mca_paffinity_hwloc_la_LDFLAGS += $(paffinity_hwloc_LDFLAGS)
mca_paffinity_hwloc_la_LIBADD += $(paffinity_hwloc_LIBS)
endif

noinst_LTLIBRARIES = $(component_noinst)
libmca_paffinity_hwloc_la_SOURCES =$(sources)
libmca_paffinity_hwloc_la_LDFLAGS = -module -avoid-version
libmca_paffinity_hwloc_la_LIBADD =
if OPAL_PAFFINITY_HWLOC_INTERNAL
# See note about above HWLOC_EMBEDDED_LDFLAGS and _LIBS.
libmca_paffinity_hwloc_la_LDFLAGS += $(HWLOC_EMBEDDED_LDFLAGS)
libmca_paffinity_hwloc_la_LIBADD += \
        $(top_ompi_builddir)/opal/mca/paffinity/hwloc/hwloc/src/libhwloc_embedded.la \
        $(HWLOC_EMBEDDED_LIBS)
else
libmca_paffinity_hwloc_la_LDFLAGS += $(paffinity_hwloc_LDFLAGS)
libmca_paffinity_hwloc_la_LIBADD += $(paffinity_hwloc_LIBS)
endif
