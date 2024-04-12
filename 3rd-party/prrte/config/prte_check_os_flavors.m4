dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2014      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl
dnl Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# PRTE_CHECK_OS_FLAVOR_SPECIFIC()
# ----------------------------------------------------
# Helper macro from PRTE-CHECK-OS-FLAVORS(), below.
# $1 = macro to look for
# $2 = suffix of env variable to set with results
AC_DEFUN([PRTE_CHECK_OS_FLAVOR_SPECIFIC],
[
    AC_MSG_CHECKING([$1])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
     [[#ifndef $1
      #this is not $1, error
      #endif
     ]])],
                      [prte_found_$2=yes],
                      [prte_found_$2=no])
    AC_MSG_RESULT([$prte_found_$2])
])dnl

# PRTE_CHECK_OS_FLAVORS()
# ----------------------------------------------------
# Try to figure out the various OS flavors out there.
#
AC_DEFUN([PRTE_CHECK_OS_FLAVORS],
[
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__NetBSD__], [netbsd])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__FreeBSD__], [freebsd])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__OpenBSD__], [openbsd])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__DragonFly__], [dragonfly])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__386BSD__], [386bsd])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__bsdi__], [bsdi])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__APPLE__], [apple])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__linux__], [linux])
    PRTE_CHECK_OS_FLAVOR_SPECIFIC([__sun__], [sun])
    AS_IF([test "$prte_found_sun" = "no"],
          PRTE_CHECK_OS_FLAVOR_SPECIFIC([__sun], [sun]))

    AS_IF([test "$prte_found_sun" = "yes"],
          [prte_have_solaris=1
           CFLAGS="$CFLAGS -D_REENTRANT"
           CPPFLAGS="$CPPFLAGS -D_REENTRANT"],
          [prte_have_solaris=0])
    AC_DEFINE_UNQUOTED([PRTE_HAVE_SOLARIS],
                       [$prte_have_solaris],
                       [Whether or not we have solaris])

    AS_IF([test "$prte_found_apple" = "yes"],
          [prte_have_apple=1],
          [prte_have_apple=0])
    AC_DEFINE_UNQUOTED([PRTE_HAVE_APPLE],
                       [$prte_have_apple],
                       [Whether or not we have apple])

    # check for sockaddr_in (a good sign we have TCP)
    AC_CHECK_HEADERS([netdb.h netinet/in.h netinet/tcp.h])
    AC_CHECK_TYPES([struct sockaddr_in],
                   [prte_found_sockaddr=yes],
                   [prte_found_sockaddr=no],
                   [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])
])dnl
