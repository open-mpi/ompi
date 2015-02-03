dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2009      The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2014      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
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
    OPAL_VAR_SCOPE_PUSH([opal_check_knem_happy opal_check_knem_$1_save_CPPFLAGS opal_check_knem_dir])
    AC_ARG_WITH([knem],
        [AC_HELP_STRING([--with-knem(=DIR)],
             [Build knem Linux kernel module support, searching for headers in DIR])])

    OPAL_CHECK_WITHDIR([knem], [$with_knem], [include/knem_io.h])
    opal_check_knem_$1_save_CPPFLAGS="$CPPFLAGS"

    AS_IF([test "$with_knem" != "no"],
          [AS_IF([test ! -z "$with_knem" && test "$with_knem" != "yes"],
                 [opal_check_knem_dir="$with_knem"])

           _OPAL_CHECK_PACKAGE_HEADER([$1],
                              [knem_io.h],
                              [$opal_check_knem_dir],
                              [opal_check_knem_happy="yes"],
                              [opal_check_knem_happy="no"])],
          [opal_check_knem_happy="no"])

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"

    # need at least version 0x0000000b
    AS_IF([test "$opal_check_knem_happy" = "yes"],
          [AC_CACHE_CHECK([for knem ABI version 0xb or later],
                          [opal_cv_knem_version_ok],
                          [AC_PREPROC_IFELSE(
                            [AC_LANG_PROGRAM([
#include <knem_io.h>
                             ],[
#if KNEM_ABI_VERSION < 0xc
#error "Version less than 0xc"
#endif
                             ])],
                            [opal_cv_knem_version_ok=yes],
                            [opal_cv_knem_version_ok=no])])])

    CPPFLAGS="$opal_check_knem_$1_save_CPPFLAGS"

    AS_IF([test "$opal_check_knem_happy" = "yes" && test "$opal_cv_knem_version_ok" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_knem" && test "$with_knem" != "no"],
                 [AC_MSG_ERROR([KNEM support requested but not found.  Aborting])])
           $3])
    OPAL_VAR_SCOPE_POP
])dnl
