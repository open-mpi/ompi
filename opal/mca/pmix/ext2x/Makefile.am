#
# Copyright (c) 2x14-2x16 Intel, Inc.  All rights reserved.
# Copyright (c) 2x15      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2x14-2x15 Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        pmix_ext2x.h \
        pmix_ext2x_component.c \
        pmix_ext2x.c \
        pmix_ext2x_client.c \
        pmix_ext2x_server_south.c \
        pmix_ext2x_server_north.c

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
mca_pmix_ext2x_la_SOURCES = $(sources)
mca_pmix_ext2x_la_CFLAGS =
mca_pmix_ext2x_la_CPPFLAGS = $(opal_pmix_ext2x_CPPFLAGS)
mca_pmix_ext2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext2x_LDFLAGS)
mca_pmix_ext2x_la_LIBADD = $(opal_pmix_ext2x_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext2x_la_SOURCES =$(sources)
libmca_pmix_ext2x_la_CFLAGS =
libmca_pmix_ext2x_la_CPPFLAGS = $(opal_pmix_ext2x_CPPFLAGS)
libmca_pmix_ext2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext2x_LDFLAGS)
libmca_pmix_ext2x_la_LIBADD = $(opal_pmix_ext2x_LIBS)
