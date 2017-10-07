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

AC_DEFUN([MCA_pmix_pif_bsdx_ipv6_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $1:$2 compile mode])
    $3="static"
    AC_MSG_RESULT([$$3])
])

# MCA_pif_config_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pif_bsdx_ipv6_CONFIG], [
    AC_CONFIG_FILES([src/mca/pif/bsdx_ipv6/Makefile])

    AC_REQUIRE([PMIX_CHECK_OS_FLAVORS])

    # If we found struct sockaddr and we're on any of the BSDs, we're
    # happy.  I.e., this:
    #if defined( __NetBSD__) || defined(__OpenBSD__) || defined(__FreeBSD__) || \
    #             defined(__386BSD__) || defined(__bsdi__) || defined(__APPLE__)
    AC_MSG_CHECKING([struct sockaddr])
    AS_IF([test "$pmix_found_sockaddr" = "yes"],
          [AC_MSG_RESULT([yes (cached)])
           AC_MSG_CHECKING([some flavor of BSD])
           AS_IF([test "$pmix_found_netbsd" = "yes" || \
                  test "$pmix_found_freebsd" = "yes" || \
                  test "$pmix_found_openbsd" = "yes" || \
                  test "$pmix_found_386bsd" = "yes" || \
                  test "$pmix_found_bsdi" = "yes" ||
                  test "$pmix_found_apple" = "yes"],
                 [AC_MSG_RESULT([yes])
                  $1],
                 [AC_MSG_RESULT([no])
                  $2])],
          [AC_MSG_RESULT([no (cached)])
           $2])
])dnl
