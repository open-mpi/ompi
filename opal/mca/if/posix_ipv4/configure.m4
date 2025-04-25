# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2025      Nanook Consulting  All rights reserved.
# Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_if_posix_ipv4_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_if_config_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_if_posix_ipv4_CONFIG], [
    AC_CONFIG_FILES([opal/mca/if/posix_ipv4/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_if_posix_ipv4_happy])
    opal_if_posix_ipv4_happy=no

    AC_REQUIRE([OAC_CHECK_OS_FLAVORS])

    # If we previously found struct sockaddr_in (we don't repeat the
    # AC CHECK_TYPES test here simply because it's cumbersome with all
    # the required #includes) and we're NOT on most of the BSDs, we're
    # happy.  I.e., this:
    #if defined(__NetBSD__) || defined(__FreeBSD__) || \
    #    defined(__OpenBSD__) || defined(__DragonFly__)
    AC_MSG_CHECKING([struct sockaddr_in again])
    AS_IF([test "$ac_cv_type_struct_sockaddr_in" = "yes"],
          [AC_MSG_RESULT([yes (OPAL cached)])
           AC_MSG_CHECKING([not NetBSD, FreeBSD, OpenBSD, or DragonFly])
           AS_IF([test "$oac_found_netbsd" = "no" && test "$oac_found_freebsd" = "no" && test "$oac_found_openbsd" = "no" && test "$oac_found_dragonfly" = "no"],
                 [AC_MSG_RESULT([yes])
                  opal_if_posix_ipv4_happy=yes],
                 [AC_MSG_RESULT([no])]
                )],
          [AC_MSG_RESULT([no (OPAL cached)])]
         )

    AS_IF([test "$opal_if_posix_ipv4_happy" = "yes"],
          [AC_CHECK_MEMBERS([struct ifreq.ifr_hwaddr], [], [],
                           [[#include <net/if.h>]])
           AC_CHECK_MEMBERS([struct ifreq.ifr_mtu], [], [],
                           [[#include <net/if.h>]])
          ])

    AS_IF([test "$opal_if_posix_ipv4_happy" = "yes"], [$1], [$2]);
])dnl
