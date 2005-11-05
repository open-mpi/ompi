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


# MCA_btl_gm_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_gm_CONFIG],[
    OMPI_CHECK_GM([btl_gm],
                     [btl_gm_happy="yes"],
                     [btl_gm_happy="no"])

    AS_IF([test "$btl_gm_happy" = "yes"],
          [btl_gm_WRAPPER_EXTRA_LDFLAGS="$btl_gm_LDFLAGS"
           btl_gm_WRAPPER_EXTRA_LIBS="$btl_gm_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build gm
    AC_SUBST([btl_gm_CFLAGS])
    AC_SUBST([btl_gm_CPPFLAGS])
    AC_SUBST([btl_gm_LDFLAGS])
    AC_SUBST([btl_gm_LIBS])
])dnl
