# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_if_bsdx_ipv4_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_if_config_CONFIG(action-if-can-compile,
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_if_bsdx_ipv4_CONFIG], [
    AC_CONFIG_FILES([opal/mca/if/bsdx_ipv4/Makefile])

    AC_REQUIRE([OPAL_CHECK_OS_FLAVORS])

    # If we found struct sockaddr and we're on any of the BSDs, we're
    # happy.  I.e., this:
    #if defined(__NetBSD__) || defined(__FreeBSD__) || \
    #    defined(__OpenBSD__) || defined(__DragonFly__)
    AC_MSG_CHECKING([struct sockaddr])
    AS_IF([test "$opal_found_sockaddr" = "yes"],
          [AC_MSG_RESULT([yes (cached)])
           AC_MSG_CHECKING([NetBSD, FreeBSD, OpenBSD, or DragonFly])
           AS_IF([test "$opal_found_netbsd" = "yes" || \
                  test "$opal_found_freebsd" = "yes" || \
                  test "$opal_found_openbsd" = "yes" || \
                  test "$opal_found_dragonfly" = "yes"],
                 [AC_MSG_RESULT([yes])
                  $1],
                 [AC_MSG_RESULT([no])
                  $2])],
          [AC_MSG_RESULT([no (cached)])
           $2])
])

