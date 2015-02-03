dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2001-2011 Mellanox Technologies Ltd. ALL RIGHTS RESERVED.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_MXM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if MXM support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MXM],[
    AC_ARG_WITH([mxm],
        [AC_HELP_STRING([--with-mxm(=DIR)],
             [Build Mellanox Messaging support])])
    OPAL_CHECK_WITHDIR([mxm], [$with_mxm], [include/mxm/api/mxm_api.h])
    AC_ARG_WITH([mxm-libdir],
        [AC_HELP_STRING([--with-mxm-libdir=DIR],
             [Search for Mellanox Messaging libraries in DIR])])
    OPAL_CHECK_WITHDIR([mxm-libdir], [$with_mxm_libdir], [libmxm.*])

    ompi_check_mxm_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_mxm_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_mxm_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_mxm" != "no"],
          [AS_IF([test ! -z "$with_mxm" && test "$with_mxm" != "yes"],
                 [
                    ompi_check_mxm_dir="$with_mxm"
                    ompi_check_mxm_libdir="$with_mxm/lib"
                 ])
           AS_IF([test ! -z "$with_mxm_libdir" && test "$with_mxm_libdir" != "yes"],
                 [ompi_check_mxm_libdir="$with_mxm_libdir"])

           ompi_check_mxm_extra_libs="-L$ompi_check_mxm_libdir"

           OPAL_CHECK_PACKAGE([$1],
                              [mxm/api/mxm_api.h],
                              [mxm],
                              [mxm_cleanup],
                              [$ompi_check_mxm_extra_libs],
                              [$ompi_check_mxm_dir],
                              [$ompi_check_mxm_libdir],
                              [ompi_check_mxm_happy="yes"],
                              [ompi_check_mxm_happy="no"])],
          [ompi_check_mxm_happy="no"])



    CPPFLAGS="$ompi_check_mxm_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_mxm_$1_save_LDFLAGS"
    LIBS="$ompi_check_mxm_$1_save_LIBS"

    AC_MSG_CHECKING(for MXM version compatibility)
    AC_REQUIRE_CPP
    old_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -I$ompi_check_mxm_dir/include"
    AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM([[#include <mxm/api/mxm_version.h>]],
                [[
#ifndef MXM_VERSION
#error "MXM Version is less than 2.1, please upgrade"
#endif
#
#if MXM_API < MXM_VERSION(2,1)
#error "MXM Version is less than 2.1, please upgrade"
#endif
                ]])],
            [ompi_mxm_version_ok="yes"],
            [ompi_mxm_version_ok="no"])

    AC_MSG_RESULT([$ompi_mxm_version_ok])
    CFLAGS=$old_CFLAGS

    AS_IF([test "$ompi_mxm_version_ok" = "no"], [ompi_check_mxm_happy="no"])

    AS_IF([test "$ompi_check_mxm_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mxm" && test "$with_mxm" != "no"],
                 [AC_MSG_ERROR([MXM support requested but not found.  Aborting])])
           $3])
])

