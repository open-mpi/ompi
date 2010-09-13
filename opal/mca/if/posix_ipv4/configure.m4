# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_if_posix_ipv4_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_if_config_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_if_posix_ipv4_CONFIG], [
    AC_REQUIRE([OPAL_CHECK_OS_FLAVORS])

    # If we found struct sockaddr and we're NOT on most of the BSDs,
    # we're happy.  I.e., if posix but not:
    #if defined(__NetBSD__) || defined(__FreeBSD__) || \
    #    defined(__OpenBSD__) || defined(__DragonFly__)
    AC_MSG_CHECKING([struct sockaddr])
    AS_IF([test "$opal_found_sockaddr" = "yes"],
          [AC_MSG_RESULT([yes (cached)])
           AC_MSG_CHECKING([not NetBSD, FreeBSD, OpenBSD, or DragonFly])
           AS_IF([test "$opal_found_netbsd" = "no" -a "$opal_found_freebsd" = "no" -a "$opal_found_openbsd" = "no" -a "$opal_found_dragonfly" = "no"],
                 [AC_MSG_RESULT([yes])
                  $1],
                 [AC_MSG_RESULT([no])
                  $2])],
          [AC_MSG_RESULT([no (cached)])
           $2])
])dnl
