#
# Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
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

if MCA_BUILD_prte_prtereachable_netlink_DSO
component_noinst =
component_install = prte_mca_prtereachable_netlink.la
else
component_noinst = libprtemca_prtereachable_netlink.la
component_install =
endif

AM_CPPFLAGS = \
        $(prte_reachable_netlink_CPPFLAGS) \
        -DPRTE_HAVE_LIBNL3=$(PRTE_HAVE_LIBNL3)

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_prtereachable_netlink_la_SOURCES = $(sources)
prte_mca_prtereachable_netlink_la_LDFLAGS = -module -avoid-version $(prte_reachable_netlink_LDFLAGS)
prte_mca_prtereachable_netlink_la_LIBADD = $(top_builddir)/src/libprrte.la \
	$(prte_reachable_netlink_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_prtereachable_netlink_la_SOURCES =$(sources)
libprtemca_prtereachable_netlink_la_LDFLAGS = -module -avoid-version $(prte_reachable_netlink_LDFLAGS)
libprtemca_prtereachable_netlink_la_LIBADD = $(prte_reachable_netlink_LIBS)
