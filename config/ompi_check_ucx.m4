# -*- shell-script -*-
#
# Copyright (C) 2015      Mellanox Technologies Ltd. ALL RIGHTS RESERVED.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_UCX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if UCX support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UCX],[
    AC_ARG_WITH([ucx],
        [AC_HELP_STRING([--with-ucx(=DIR)],
             [Build with Unified Communication X library support])])
    OPAL_CHECK_WITHDIR([ucx], [$with_ucx], [include/ucp/api/ucp.h])
    AC_ARG_WITH([ucx-libdir],
        [AC_HELP_STRING([--with-ucx-libdir=DIR],
             [Search for Unified Communication X libraries in DIR])])
    OPAL_CHECK_WITHDIR([ucx-libdir], [$with_ucx_libdir], [libucp.*])

    ompi_check_ucx_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_ucx_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_ucx_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_ucx" != "no"],
          [AS_IF([test ! -z "$with_ucx" && test "$with_ucx" != "yes"],
                 [
                    ompi_check_ucx_dir="$with_ucx"
                    ompi_check_ucx_libdir="$with_ucx/lib"
                 ])
           AS_IF([test ! -z "$with_ucx_libdir" && test "$with_ucx_libdir" != "yes"],
                 [ompi_check_ucx_libdir="$with_ucx_libdir"])

           ompi_check_ucx_extra_libs="-L$ompi_check_ucx_libdir"

           OPAL_CHECK_PACKAGE([$1],
                              [ucp/api/ucp.h],
                              [ucp],
                              [ucp_cleanup],
                              [$ompi_check_ucx_extra_libs],
                              [$ompi_check_ucx_dir],
                              [$ompi_check_ucx_libdir],
                              [ompi_check_ucx_happy="yes"],
                              [ompi_check_ucx_happy="no"])],
          [ompi_check_ucx_happy="no"])



    CPPFLAGS="$ompi_check_ucx_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_ucx_$1_save_LDFLAGS"
    LIBS="$ompi_check_ucx_$1_save_LIBS"

    AC_MSG_CHECKING(for UCX version compatibility)
    AC_REQUIRE_CPP
    old_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -I$ompi_check_ucx_dir/include"
    AC_COMPILE_IFELSE(
            [AC_LANG_PROGRAM([[#include <uct/api/version.h>]],
                [[
                ]])],
            [ompi_ucx_version_ok="yes"],
            [ompi_ucx_version_ok="no"])

    AC_MSG_RESULT([$ompi_ucx_version_ok])
    CFLAGS=$old_CFLAGS

    AS_IF([test "$ompi_ucx_version_ok" = "no"], [ompi_check_ucx_happy="no"])

    AS_IF([test "$ompi_check_ucx_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ucx" && test "$with_ucx" != "no"],
                 [AC_MSG_ERROR([UCX support requested but not found.  Aborting])])
           $3])
])

