#
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = autogen.subdirs

dist_opaldata_DATA = help-pmix-pmix2x.txt

SUBDIRS = pmix

sources = \
        pmix2x.h \
        pmix2x_component.c \
        pmix2x.c \
        pmix2x_client.c \
        pmix2x_local.c \
        pmix2x_server_south.c \
        pmix2x_server_north.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_pmix2x_DSO
component_noinst =
component_install = mca_pmix_pmix2x.la
else
component_noinst = libmca_pmix_pmix2x.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_pmix2x_la_SOURCES = $(sources)
mca_pmix_pmix2x_la_CFLAGS = $(opal_pmix_pmix2x_CFLAGS)
mca_pmix_pmix2x_la_CPPFLAGS = \
        -I$(srcdir)/pmix/include $(opal_pmix_pmix2x_CPPFLAGS)
mca_pmix_pmix2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_pmix2x_LDFLAGS)
mca_pmix_pmix2x_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmix_pmix2x_LIBS)
mca_pmix_pmix2x_la_DEPENDENCIES = $(opal_pmix_pmix2x_DEPENDENCIES)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_pmix2x_la_SOURCES =$(sources)
libmca_pmix_pmix2x_la_CFLAGS = $(opal_pmix_pmix2x_CFLAGS)
libmca_pmix_pmix2x_la_CPPFLAGS = $(opal_pmix_pmix2x_CPPFLAGS)
libmca_pmix_pmix2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_pmix2x_LDFLAGS)
libmca_pmix_pmix2x_la_LIBADD = $(opal_pmix_pmix2x_LIBS)
libmca_pmix_pmix2x_la_DEPENDENCIES = $(opal_pmix_pmix2x_DEPENDENCIES)
