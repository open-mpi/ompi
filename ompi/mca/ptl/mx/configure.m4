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


# MCA_ptl_mx_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ptl_mx_CONFIG],[
    OMPI_CHECK_MX([ptl_mx],
                     [ptl_mx_happy="yes"],
                     [ptl_mx_happy="no"])

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

