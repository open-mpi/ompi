# -*- shell-script -*-
#
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_reachable_netlink_CONFIG([action-if-can-compile],
#                                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_reachable_netlink_CONFIG],[
    AC_CONFIG_FILES([opal/mca/reachable/netlink/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_reachable_netlink_happy])

    # this is terrible, but libnl-1 and libnl-3 are incompatible in
    # weird ways, and once there are libraries in LIBS for one, the
    # other is hard to get right.  So if someone has already decided
    # we have libnl version 1, get out.  Otherwise, see if we have
    # libnl-3, which is the only version supported by the netlink
    # component.
    AS_IF([test $opal_libnl_version -eq 3],
        [# Use the libnl base options for the route search
         with_libnl_route=${with_libnl}
         with_libnl_route_libdir=${with_libnl_libdir}
         with_libnl_route_incdir=${with_libnl_incdir}

         # The first argument of OAC_CHECK_PACKAGE must be a valid
         # shell variable name, which means no dashs.  Deal with that
         # by being explicit with our module.
         m4_define([libnl_route_pkgconfig_module], [libnl-route-3.0])
         OAC_CHECK_PACKAGE([libnl_route],
                           [reachable_netlink],
                           [netlink/route/route.h],
                           [nl-route-3 nl-3],
                           [rtnl_route_get],
                           [opal_reachable_netlink_happy=1],
                           [opal_reachable_netlink_happy=0])

         # See if we have linux/netlink.h, which is part of the kernal
         # ABI headers and not the libnl3 package (so we assume are in
         # default includes, as opposed to the libnl directory.
         AS_IF([test ${opal_reachable_netlink_happy} -eq 1],
                [AC_CHECK_HEADER([linux/netlink.h], [],
                   [opal_reachable_netlink_happy=0], [
#include <sys/types.h>
#include <net/if.h>
                 ])])],
        [AC_MSG_NOTICE([Disabling component because libnl v1 already linked])
         opal_reachable_netlink_happy=0])

    AS_IF([test $opal_reachable_netlink_happy -eq 1],
          [$1],
          [$2])

    AC_SUBST([reachable_netlink_CPPFLAGS])
    AC_SUBST([reachable_netlink_LDFLAGS])
    AC_SUBST([reachable_netlink_LIBS])

    OPAL_VAR_SCOPE_POP()
])
