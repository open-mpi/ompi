dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2014      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_CHECK_OS_FLAVOR_SPECIFIC()
# ----------------------------------------------------
# Helper macro from OPAL-CHECK-OS-FLAVORS(), below.
# $1 = macro to look for
# $2 = suffix of env variable to set with results
AC_DEFUN([OPAL_CHECK_OS_FLAVOR_SPECIFIC],
[
    AC_MSG_CHECKING([$1])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
     [[#ifndef $1
      error: this isnt $1
      #endif
     ]])],
                      [opal_found_$2=yes],
                      [opal_found_$2=no])
    AC_MSG_RESULT([$opal_found_$2])
])dnl

# OPAL_CHECK_OS_FLAVORS()
# ----------------------------------------------------
# Try to figure out the various OS flavors out there.
#
AC_DEFUN([OPAL_CHECK_OS_FLAVORS],
[
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__NetBSD__], [netbsd])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__FreeBSD__], [freebsd])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__OpenBSD__], [openbsd])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__DragonFly__], [dragonfly])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__386BSD__], [386bsd])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__bsdi__], [bsdi])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__APPLE__], [apple])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__linux__], [linux])
    OPAL_CHECK_OS_FLAVOR_SPECIFIC([__sun__], [sun])
    AS_IF([test "$opal_found_sun" = "no"],
          OPAL_CHECK_OS_FLAVOR_SPECIFIC([__sun], [sun]))

    AS_IF([test "$opal_found_sun" = "yes"],
          [opal_have_solaris=1
           CFLAGS="$CFLAGS -D_REENTRANT"
           CPPFLAGS="$CPPFLAGS -D_REENTRANT"],
          [opal_have_solaris=0])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_SOLARIS],
                       [$opal_have_solaris],
                       [Whether or not we have solaris])

    # check for sockaddr_in (a good sign we have TCP)
    AC_CHECK_HEADERS([netdb.h netinet/in.h netinet/tcp.h])
    AC_CHECK_TYPES([struct sockaddr_in], 
                   [opal_found_sockaddr=yes],
                   [opal_found_sockaddr=no], 
                   [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])
])dnl
