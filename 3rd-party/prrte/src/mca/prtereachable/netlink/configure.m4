# -*- shell-script -*-
#
# Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2024      Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_prte_prtereachable_netlink_CONFIG([action-if-can-compile],
#                                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_prte_prtereachable_netlink_CONFIG],[
    AC_CONFIG_FILES([src/mca/prtereachable/netlink/Makefile])

    PRTE_VAR_SCOPE_PUSH([prte_reachable_netlink_happy])

    prte_reachable_netlink_happy=1

    # ugly hack to deal with potentially alternate locations for
    # libnl3 headers.  Note that if the pkg-config file is found,
    # this ugly hack won't be used.
    AS_IF([test -n "$with_libnl_route_3_0_incdir"],
          [# skip check if someone above set incdir],
          [test -d "/usr/include/libnl3"],
          [with_libnl_route_3_0_incdir="/usr/include/libnl3"],
          [test -d "/usr/local/include/libnl3"],
          [with_libnl_route_3_0_incdir="/usr/local/include/libnl3"])

    # note we need the route package, not the basic package
    # The first argument of OAC_CHECK_PACKAGE must be a valid
    # shell variable name, which means no dashs.  Deal with that
    # by being explicit with our module.
    m4_define([libnl_route_pkgconfig_module], [libnl-route-3.0])
    OAC_CHECK_PACKAGE([libnl_route],
                      [prte_reachable_netlink],
                      [netlink/route/route.h],
                      [nl-route-3 nl-3],
                      [rtnl_route_get],
                      [prte_reachable_netlink_happy=1],
                      [prte_reachable_netlink_happy=0])

    AS_IF([test ${prte_reachable_netlink_happy} -eq 1],
          [AC_CHECK_HEADER([linux/netlink.h], [],
                           [prte_reachable_netlink_happy=0], [
#include <sys/types.h>
#include <net/if.h>
])])

    AS_IF([test $prte_reachable_netlink_happy -eq 1],
          [$1],
          [$2])

    AC_SUBST([prte_reachable_netlink_CPPFLAGS])
    AC_SUBST([prte_reachable_netlink_LDFLAGS])
    AC_SUBST([prte_reachable_netlink_LIBS])

    PRTE_VAR_SCOPE_POP()
])
