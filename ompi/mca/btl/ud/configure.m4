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
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# MCA_btl_ud_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_ud_CONFIG],[
    OMPI_CHECK_OPENIB([btl_ud],
                     [btl_ud_happy="yes"],
                     [btl_ud_happy="no"])

    AS_IF([test "$btl_ud_happy" = "yes"],
          [btl_ud_WRAPPER_EXTRA_LDFLAGS="$btl_ud_LDFLAGS"
           btl_ud_WRAPPER_EXTRA_LIBS="$btl_ud_LIBS"
           $1],
          [$2])


    # substitute in the things needed to build ud
    AC_SUBST([btl_ud_CFLAGS])
    AC_SUBST([btl_ud_CPPFLAGS])
    AC_SUBST([btl_ud_LDFLAGS])
    AC_SUBST([btl_ud_LIBS])
])dnl
