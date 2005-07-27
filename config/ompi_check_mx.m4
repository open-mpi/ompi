# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


AC_DEFUN([_OMPI_CHECK_MX_CONFIG],[
    u_OMPI_CHECK_MX_CONFIG_SAVE_CPPFLAGS="$CPPFLAGS"
    u_OMPI_CHECK_MX_CONFIG_SAVE_LDFLAGS="$LDFLAGS"
    u_OMPI_CHECK_MX_CONFIG_SAVE_LIBS="$LIBS"

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
    LIBS="$LIBS $$1_LIBS"

    #
    # See if we have MX_API. OpenMPI require a MX version bigger than the first MX relase (0x300)
    #
    AC_MSG_CHECKING([for MX version 1.0 or later])
    AC_TRY_COMPILE([#include <myriexpress.h],
        [#if MX_API < 0x300
         #error "Version less than 0x300"
         #endif],
         [have_recent_api="yes"],
         [have_recent_api="no"])
    AC_MSG_RESULT([$have_recent_api])

    AC_MSG_CHECKING(for MX_API)
    if test x"$have_recent_api" = "xyes"; then
        AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_API_VERSION], $mx_api_ver,
            [Version of the MX API to use])
        unset mx_api_ver have_mx_api_ver_msg found val msg
    fi
    CPPFLAGS="$u_OMPI_CHECK_MX_CONFIG_SAVE_CPPFLAGS"
    LDFLAGS="$u_OMPI_CHECK_MX_CONFIG_SAVE_LDFLAGS"
    LIBS="$u_OMPI_CHECK_MX_CONFIG_SAVE_LIBS"
])dnl

# OMPI_CHECK_MX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if MX support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MX],[
    AC_ARG_WITH([mx],
                [AC_HELP_STRING([--with-btl-mx=MX_DIR],
                                [Additional directory to search for MX installation])])
    AC_ARG_WITH([mx-libdir],
       [AC_HELP_STRING([--with-btl-mx-libdir=IBLIBDIR],
                       [directory where the MX library can be found, if it is not in MX_DIR/lib or MX_DIR/lib64])])

    AS_IF([test ! -z "$with_btl_mx" -a "$with_btl_mx" != "yes"],
          [ompi_check_mx_dir="$with_btl_mx"])
    AS_IF([test ! -z "$with_btl_mx_libdir" -a "$with_btl_mx_libdir" != "yes"],
          [ompi_check_mx_libdir="$with_btl_mx_libdir"])

    OMPI_CHECK_PACKAGE([$1],
                       [myriexpress.h],
                       [myriexpress],
                       [mx_finalize],
                       [],
                       [$ompi_check_mx_dir],
                       [$ompi_check_mx_libdir],
                       [ompi_check_mx_happy="yes"],
                       [ompi_check_mx_happy="no"])

    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_CONFIG($1)
           $2],
          [AS_IF([test ! -z "$with_btl_mx" -a "$with_btl_mx" != "no"],
                 [AC_MSG_ERROR([MX support requested but not found.  Aborting])])
           $3])
])

