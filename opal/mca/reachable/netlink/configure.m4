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

    opal_reachable_netlink_happy=1
    AC_CHECK_HEADER([linux/netlink.h], [],
                    [opal_reachable_netlink_happy=0], [
#include <sys/types.h>
#include <net/if.h>
])

    # this is terrible, but libnl-1 and libnl-3 are incompatible in
    # weird ways, and once there are libraries in LIBS for one, the
    # other is hard to get right.  So if someone has already decided
    # we have libnl version 1, get out.  Otherwise, see if we have
    # libnl-3, which is the only version supported by the netlink
    # component.
    AS_IF([test $opal_libnl_version -eq 1],
	  [opal_reachable_netlink_happy=0],
          [OPAL_CHECK_LIBNL_V3([$opal_libnl_location],
			       [opal_reachable_netlink])
	   AS_IF([test "$OPAL_HAVE_LIBNL3" != "1"],
		 [opal_reachable_netlink_happy=0])])

    AS_IF([test $opal_reachable_netlink_happy -eq 1],
          [$1],
          [$2])

    AC_SUBST([opal_reachable_netlink_CPPFLAGS])
    AC_SUBST([opal_reachable_netlink_LDFLAGS])
    AC_SUBST([opal_reachable_netlink_LIBS])

    OPAL_VAR_SCOPE_POP()
])
