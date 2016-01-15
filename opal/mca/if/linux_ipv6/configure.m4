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

AC_DEFUN([MCA_opal_if_linux_ipv6_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_if_config_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_if_linux_ipv6_CONFIG], [
    AC_CONFIG_FILES([opal/mca/if/linux_ipv6/Makefile])

    AC_REQUIRE([OPAL_CHECK_OS_FLAVORS])

    AC_MSG_CHECKING([if we are on Linux with TCP])
    # If we have struct sockaddr and we're on Linux, then we're
    # happy.
    AS_IF([test "$opal_found_sockaddr" = "yes" && test "$opal_found_linux" = "yes"],
          [AC_MSG_RESULT([yes])
           $1],
          [AC_MSG_RESULT([no])
           $2])
])dnl
