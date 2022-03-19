dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2009      The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2014-2016 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_CHECK_KNEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if knem support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OPAL_CHECK_KNEM],[
    OPAL_VAR_SCOPE_PUSH([opal_check_knem_happy opal_check_knem_CPPFLAGS_save])

    AC_ARG_WITH([knem],
                [AS_HELP_STRING([--with-knem(=DIR)],
                                [Build knem Linux kernel module support, searching for headers in DIR/include])])

    opal_check_knem_CPPFLAGS_save="${CPPFLAGS}"

    AS_IF([test -n "${with_knem}" -a "${with_knem}" != "yes" -a "${with_knem}" != "no"],
          [$1_CPPFLAGS="-I${with_knem}/include"
           CPPFLAGS="$CPPFLAGS ${$1_CPPFLAGS}"])

    AC_CHECK_HEADER([knem_io.h], [opal_check_knem_happy="yes"], [opal_check_knem_happy="no"])

    # need at least version 0x0000000b
    AS_IF([test "$opal_check_knem_happy" = "yes"],
          [AC_CACHE_CHECK([for recent vesion of knem ABI],
              [opal_check_knem_cv_abi_version],
              [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <knem_io.h>
                                              ]], [[
#if KNEM_ABI_VERSION < 0xc
#error "Version less than 0xc"
#endif
                                              ]])],
                              [opal_check_knem_cv_abi_version=yes],
                              [opal_check_knem_cv_abi_version=no])])
               AS_IF([test "${opal_check_knem_cv_abi_version}" = "no"],
                     [opal_check_knem_happy=no])])

    CPPFLAGS="${opal_check_knem_CPPFLAGS_save}"

    OPAL_SUMMARY_ADD([Transports], [Shared memory/Linux KNEM], [], [$opal_check_knem_happy])

    AS_IF([test "$opal_check_knem_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_knem" && test "$with_knem" != "no"],
                 [AC_MSG_ERROR([KNEM support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])dnl
