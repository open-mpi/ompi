#
# Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix_ext.h \
        pmix_ext_component.c \
        pmix_ext.c \
        pmix_ext_client.c \
        pmix_ext_server_south.c \
        pmix_ext_server_north.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_external_DSO
component_noinst =
component_install = mca_pmix_external.la
else
component_noinst = libmca_pmix_external.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_external_la_SOURCES = $(sources)
mca_pmix_external_la_CFLAGS =
mca_pmix_external_la_CPPFLAGS = $(opal_pmix_ext_CPPFLAGS)
mca_pmix_external_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext_LDFLAGS)
mca_pmix_external_la_LIBADD = $(opal_pmix_ext_LIBS) \
        $(OPAL_TOP_BUILDDIR)/opal/mca/event/lib@OPAL_LIB_PREFIX@mca_event.la \
        $(OPAL_TOP_BUILDDIR)/opal/mca/hwloc/lib@OPAL_LIB_PREFIX@mca_hwloc.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_external_la_SOURCES =$(sources)
libmca_pmix_external_la_CFLAGS =
libmca_pmix_external_la_CPPFLAGS = $(opal_pmix_ext_CPPFLAGS)
libmca_pmix_external_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext_LDFLAGS)
libmca_pmix_external_la_LIBADD = $(opal_pmix_ext_LIBS)
