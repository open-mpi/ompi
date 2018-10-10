#
# Copyright (c) 2014-2018 Intel, Inc.  All rights reserved.
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

dist_opaldata_DATA = help-pmix-ext2x.txt

sources = \
        ext2x_local.c \
        ext2x.c \
        ext2x_client.c \
        ext2x_component.c \
        ext2x_server_north.c \
        ext2x_server_south.c

headers = \
        ext2x.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_ext2x_DSO
component_noinst =
component_install = mca_pmix_ext2x.la
else
component_noinst = libmca_pmix_ext2x.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_ext2x_la_SOURCES = $(sources) $(headers)
mca_pmix_ext2x_la_CFLAGS = $(opal_pmix_ext2x_CFLAGS)
mca_pmix_ext2x_la_CPPFLAGS = $(opal_pmix_ext2x_CPPFLAGS)
mca_pmix_ext2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext2x_LDFLAGS)
mca_pmix_ext2x_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmix_ext2x_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext2x_la_SOURCES = $(sources) $(headers)
libmca_pmix_ext2x_la_CFLAGS = $(opal_pmix_ext2x_CFLAGS)
libmca_pmix_ext2x_la_CPPFLAGS = $(opal_pmix_ext2x_CPPFLAGS)
libmca_pmix_ext2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext2x_LDFLAGS)
libmca_pmix_ext2x_la_LIBADD = $(opal_pmix_ext2x_LIBS)
