#
# Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = autogen.subdirs

SUBDIRS = pmix

sources = \
        pmix120.h \
        pmix_pmix120_component.c \
        pmix_pmix120.c \
        pmix120_client.c \
        pmix120_server_south.c \
        pmix120_server_north.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_pmix120_DSO
component_noinst =
component_install = mca_pmix_pmix120.la
else
component_noinst = libmca_pmix_pmix120.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_pmix120_la_SOURCES = $(sources)
mca_pmix_pmix120_la_CFLAGS = $(opal_pmix_pmix120_CFLAGS)
mca_pmix_pmix120_la_CPPFLAGS = \
        -I$(srcdir)/pmix/include $(opal_pmix_pmix120_CPPFLAGS)
mca_pmix_pmix120_la_LDFLAGS = -module -avoid-version $(opal_pmix_pmix120_LDFLAGS)
mca_pmix_pmix120_la_LIBADD = $(opal_pmix_pmix120_LIBS)
mca_pmix_pmix120_la_DEPENDENCIES = $(mca_pmix_pmix120_la_LIBADD)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_pmix120_la_SOURCES =$(sources)
libmca_pmix_pmix120_la_CFLAGS = $(opal_pmix_pmix120_CFLAGS)
libmca_pmix_pmix120_la_CPPFLAGS = -I$(srcdir)/pmix/include $(opal_pmix_pmix120_CPPFLAGS)
libmca_pmix_pmix120_la_LDFLAGS = -module -avoid-version $(opal_pmix_pmix120_LDFLAGS)
libmca_pmix_pmix120_la_LIBADD = $(opal_pmix_pmix120_LIBS)
libmca_pmix_pmix120_la_DEPENDENCIES = $(libmca_pmix_pmix120_la_LIBADD)
