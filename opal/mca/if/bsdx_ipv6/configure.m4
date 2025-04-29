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

AC_DEFUN([MCA_opal_if_bsdx_ipv6_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_if_config_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_if_bsdx_ipv6_CONFIG], [
    AC_CONFIG_FILES([opal/mca/if/bsdx_ipv6/Makefile])

    AC_REQUIRE([OAC_CHECK_OS_FLAVORS])

    # If we previously found struct sockaddr_in6 (we don't repeat the
    # AC CHECK_TYPES test here simply because it's cumbersome with all
    # the required #includes) and we're on any of the BSDs, we're
    # happy.  I.e., this:
    #if defined( __NetBSD__) || defined(__OpenBSD__) || defined(__FreeBSD__) || \
    #             defined(__386BSD__) || defined(__bsdi__) || defined(__APPLE__)
    AC_MSG_CHECKING([struct sockaddr_in6 again])
    AS_IF([test "$ac_cv_type_struct_sockaddr_in6" = "yes"],
          [AC_MSG_RESULT([yes (OPAL cached)])
           AC_MSG_CHECKING([some flavor of BSD])
           AS_IF([test "$oac_found_netbsd" = "yes" || \
                  test "$oac_found_freebsd" = "yes" || \
                  test "$oac_found_openbsd" = "yes" || \
                  test "$oac_found_386bsd" = "yes" || \
                  test "$oac_found_bsdi" = "yes" ||
                  test "$oac_found_apple" = "yes"],
                 [AC_MSG_RESULT([yes])
                  $1],
                 [AC_MSG_RESULT([no])
                  $2])],
          [AC_MSG_RESULT([no (OPAL cached)])
           $2])
])dnl
