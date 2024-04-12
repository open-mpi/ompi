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

# OAC_CHECK_OS_FLAVOR_SPECIFIC()
# ----------------------------------------------------
# Helper macro from OAC-CHECK-OS-FLAVORS(), below.
# $1 = macro to look for
# $2 = suffix of env variable to set with results
AC_DEFUN([OAC_CHECK_OS_FLAVOR_SPECIFIC],
[
    AC_MSG_CHECKING([$1])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
     [[#ifndef $1
      #this is not $1, error
      #endif
     ]])],
                      [oac_found_$2=yes],
                      [oac_found_$2=no])
    AC_MSG_RESULT([$oac_found_$2])
])dnl

# OAC_CHECK_OS_FLAVORS()
# ----------------------------------------------------
# Try to figure out the various OS flavors out there.
#
AC_DEFUN([OAC_CHECK_OS_FLAVORS],
[
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__NetBSD__], [netbsd])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__FreeBSD__], [freebsd])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__OpenBSD__], [openbsd])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__DragonFly__], [dragonfly])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__386BSD__], [386bsd])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__bsdi__], [bsdi])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__APPLE__], [apple])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__linux__], [linux])
    OAC_CHECK_OS_FLAVOR_SPECIFIC([__sun__], [sun])
    AS_IF([test "$oac_found_sun" = "no"],
          OAC_CHECK_OS_FLAVOR_SPECIFIC([__sun], [sun]))

    AS_IF([test "$oac_found_sun" = "yes"],
          [oac_have_solaris=1
           CFLAGS="$CFLAGS -D_REENTRANT"
           CPPFLAGS="$CPPFLAGS -D_REENTRANT"],
          [oac_have_solaris=0])
    AC_DEFINE_UNQUOTED([OAC_HAVE_SOLARIS],
                       [$oac_have_solaris],
                       [Whether or not we have solaris])
    AM_CONDITIONAL(OAC_HAVE_SOLARIS, test "$oac_have_solaris" = "1")

    AS_IF([test "$oac_found_apple" = "yes"],
          [oac_have_apple=1],
          [oac_have_apple=0])
    AC_DEFINE_UNQUOTED([OAC_HAVE_APPLE],
                       [$oac_have_apple],
                       [Whether or not we have apple])
    AM_CONDITIONAL(OAC_HAVE_APPLE, test "$oac_have_apple" = "1")

])dnl
