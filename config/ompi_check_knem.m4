# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_KNEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if knem support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_KNEM],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_knem_happy ompi_check_knem_$1_save_CPPFLAGS ompi_check_knem_dir])
    AC_ARG_WITH([knem],
        [AC_HELP_STRING([--with-knem(=DIR)],
             [Build knem Linux kernel module support, searching for headers in DIR])])

    OMPI_CHECK_WITHDIR([knem], [$with_knem], [include/knem_io.h])
    ompi_check_knem_$1_save_CPPFLAGS="$CPPFLAGS"

    AS_IF([test "$with_knem" != "no"],
          [AS_IF([test ! -z "$with_knem" -a "$with_knem" != "yes"],
                 [ompi_check_knem_dir="$with_knem"])

           _OMPI_CHECK_PACKAGE_HEADER([$1],
                              [knem_io.h],
                              [$ompi_check_knem_dir],
                              [ompi_check_knem_happy="yes"],
                              [ompi_check_knem_happy="no"])],
          [ompi_check_knem_happy="no"])

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"

    # need at least version 0x0000000b
    AS_IF([test "$ompi_check_knem_happy" = "yes"],
          [AC_CACHE_CHECK([for knem ABI version 0xb or later],
                          [ompi_cv_knem_version_ok],
                          [AC_PREPROC_IFELSE(
                            [AC_LANG_PROGRAM([
#include <knem_io.h>
                             ],[
#if KNEM_ABI_VERSION < 0xc
#error "Version less than 0xc"
#endif
                             ])],
                            [ompi_cv_knem_version_ok=yes],
                            [ompi_cv_knem_version_ok=no])])])

    CPPFLAGS="$ompi_check_knem_$1_save_CPPFLAGS"

    AS_IF([test "$ompi_check_knem_happy" = "yes" -a "$ompi_cv_knem_version_ok" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_knem" -a "$with_knem" != "no"],
                 [AC_MSG_ERROR([KNEM support requested but not found.  Aborting])])
           $3])
    OPAL_VAR_SCOPE_POP
])dnl
