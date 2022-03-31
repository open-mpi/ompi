#
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        reachable_netlink.h \
        reachable_netlink_component.c \
        reachable_netlink_module.c \
        libnl3_utils.h \
        libnl_utils.h \
        reachable_netlink_utils_common.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_reachable_netlink_DSO
component_noinst =
component_install = mca_reachable_netlink.la
else
component_noinst = libmca_reachable_netlink.la
component_install =
endif

AM_CPPFLAGS = \
        $(reachable_netlink_CPPFLAGS) \
        -DOPAL_HAVE_LIBNL3=$(OPAL_HAVE_LIBNL3)

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_reachable_netlink_la_SOURCES = $(sources)
mca_reachable_netlink_la_LDFLAGS = -module -avoid-version $(reachable_netlink_LDFLAGS)
mca_reachable_netlink_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_NAME@.la \
	$(reachable_netlink_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_reachable_netlink_la_SOURCES =$(sources)
libmca_reachable_netlink_la_LDFLAGS = -module -avoid-version $(reachable_netlink_LDFLAGS)
libmca_reachable_netlink_la_LIBADD = $(reachable_netlink_LIBS)
