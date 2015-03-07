#
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
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
        libnl1_utils.h \
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
        $(opal_reachable_netlink_LIBNL_CPPFLAGS) \
        -DHAVE_LIBNL3=$(HAVE_LIBNL3)

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_reachable_netlink_la_SOURCES = $(sources)
mca_reachable_netlink_la_LDFLAGS = -module -avoid-version
mca_reachable_netlink_la_LIBADD = $(opal_reachable_netlink_LIBNL_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_reachable_netlink_la_SOURCES =$(sources)
libmca_reachable_netlink_la_LDFLAGS = -module -avoid-version
libmca_reachable_netlink_la_LIBADD = $(opal_reachable_netlink_LIBNL_LIBS)
