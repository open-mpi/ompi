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

    AC_REQUIRE([OAC_CHECK_OS_FLAVORS])

    # If we previously found struct sockaddr_in6 (we don't repeat the
    # AC CHECK_TYPES test here simply because it's cumbersome with all
    # the required #includes) and we're on Linux, we're happy.
    AC_MSG_CHECKING([for Linux with struct sockaddr_in6])
    AS_IF([test "$ac_cv_type_struct_sockaddr_in6" = "yes" && test "$oac_found_linux" = "yes"],
          [AC_MSG_RESULT([yes])
           $1],
          [AC_MSG_RESULT([no])
           $2])
])dnl
