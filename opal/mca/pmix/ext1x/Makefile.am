#
# Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix1x.h \
        pmix1x_component.c \
        pmix1x.c \
        pmix1x_client.c \
        pmix1x_server_south.c \
        pmix1x_server_north.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_ext1x_DSO
component_noinst =
component_install = mca_pmix_ext1x.la
else
component_noinst = libmca_pmix_ext1x.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_ext1x_la_SOURCES = $(sources)
mca_pmix_ext1x_la_CFLAGS =
mca_pmix_ext1x_la_CPPFLAGS = $(opal_pmix_ext1x_CPPFLAGS)
mca_pmix_ext1x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext1x_LDFLAGS)
mca_pmix_ext1x_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmix_ext1x_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext1x_la_SOURCES =$(sources)
libmca_pmix_ext1x_la_CFLAGS =
libmca_pmix_ext1x_la_CPPFLAGS = $(opal_pmix_ext1x_CPPFLAGS)
libmca_pmix_ext1x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext1x_LDFLAGS)
libmca_pmix_ext1x_la_LIBADD = $(opal_pmix_ext1x_LIBS)
