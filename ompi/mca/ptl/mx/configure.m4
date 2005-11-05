# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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


# MCA_ptl_mx_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ptl_mx_CONFIG],[
    OMPI_CHECK_MX([ptl_mx],
                     [ptl_mx_happy="yes"],
                     [ptl_mx_happy="no"])

    if [ test "$ptl_mx_happy" = "yes" ]; then
        #
        # Save a copy of the flags
        #
        ompi_check_mx_callback_CPPFLAGS="$CPPFLAGS"
        ompi_check_mx_callback_LDFLAGS="$LDFLAGS"
        ompi_check_mx_callback_LIBS="$LIBS"
        #
        # Set the value allowing MX compilation
        #
        CPPFLAGS="$CPPFLAGS $ptl_mx_CPPFLAGS"
        LDFLAGS="$LDFLAGS $ptl_mx_LDFLAGS"
        LIBS="$LIBS $ptl_mx_LIBS"

        AC_MSG_CHECKING([for a MX version with mx_register_match_callback])
        AC_TRY_COMPILE([#include <myriexpress.h>],
             [mx_register_match_callback(0, 0, 0);],
             [ptl_mx_happy="yes"],
             [ptl_mx_happy="no"])
        AC_MSG_RESULT([$ptl_mx_happy])
        #
        # Restore the original flags
        #
        CPPFLAGS="$ompi_check_mx_callback_CPPFLAGS"
        LDFLAGS="$ompi_check_mx_callback_LDFLAGS"
        LIBS="$ompi_check_mx_callback_LIBS"
    fi

    AS_IF([test "$ptl_mx_happy" = "yes"],
          [ptl_mx_WRAPPER_EXTRA_LDFLAGS="$ptl_mx_LDFLAGS"
           ptl_mx_WRAPPER_EXTRA_LIBS="$ptl_mx_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build mx
    AC_SUBST([ptl_mx_CFLAGS])
    AC_SUBST([ptl_mx_CPPFLAGS])
    AC_SUBST([ptl_mx_LDFLAGS])
    AC_SUBST([ptl_mx_LIBS])
])dnl

