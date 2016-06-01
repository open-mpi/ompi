#
# Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
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
        pmix_ext20.h \
        pmix_ext20_component.c \
        pmix_ext20.c \
        pmix_ext20_client.c \
        pmix_ext20_server_south.c \
        pmix_ext20_server_north.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_ext20_DSO
component_noinst =
component_install = mca_pmix_ext20.la
else
component_noinst = libmca_pmix_ext20.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_ext20_la_SOURCES = $(sources)
mca_pmix_ext20_la_CFLAGS =
mca_pmix_ext20_la_CPPFLAGS = $(opal_pmix_ext20_CPPFLAGS)
mca_pmix_ext20_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext20_LDFLAGS)
mca_pmix_ext20_la_LIBADD = $(opal_pmix_ext20_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext20_la_SOURCES =$(sources)
libmca_pmix_ext20_la_CFLAGS =
libmca_pmix_ext20_la_CPPFLAGS = $(opal_pmix_ext20_CPPFLAGS)
libmca_pmix_ext20_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext20_LDFLAGS)
libmca_pmix_ext20_la_LIBADD = $(opal_pmix_ext20_LIBS)
