# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_pmix_pif_posix_ipv4_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $1:$2 compile mode])
    $3="static"
    AC_MSG_RESULT([$$3])
])

# MCA_pif_config_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pif_posix_ipv4_CONFIG], [
    AC_CONFIG_FILES([src/mca/pif/posix_ipv4/Makefile])

    PMIX_VAR_SCOPE_PUSH([pmix_pif_posix_ipv4_happy])
    pmix_pif_posix_ipv4_happy=no

    AC_REQUIRE([PMIX_CHECK_OS_FLAVORS])

    # If we found struct sockaddr and we're NOT on most of the BSDs,
    # we're happy.  I.e., if posix but not:
    #if defined(__NetBSD__) || defined(__FreeBSD__) || \
    #    defined(__OpenBSD__) || defined(__DragonFly__)
    AC_MSG_CHECKING([struct sockaddr])
    AS_IF([test "$pmix_found_sockaddr" = "yes"],
          [AC_MSG_RESULT([yes (cached)])
           AC_MSG_CHECKING([not NetBSD, FreeBSD, OpenBSD, or DragonFly])
           AS_IF([test "$pmix_found_netbsd" = "no" && test "$pmix_found_freebsd" = "no" && test "$pmix_found_openbsd" = "no" && test "$pmix_found_dragonfly" = "no"],
                 [AC_MSG_RESULT([yes])
                  pmix_pif_posix_ipv4_happy=yes],
                 [AC_MSG_RESULT([no])]
                )],
          [AC_MSG_RESULT([no (cached)])]
         )

    AS_IF([test "$pmix_pif_posix_ipv4_happy" = "yes"],
          [AC_CHECK_MEMBERS([struct ifreq.ifr_hwaddr], [], [],
                           [[#include <net/if.h>]])
           AC_CHECK_MEMBERS([struct ifreq.ifr_mtu], [], [],
                           [[#include <net/if.h>]])
          ])

    AS_IF([test "$pmix_pif_posix_ipv4_happy" = "yes"], [$1], [$2]);
])dnl
