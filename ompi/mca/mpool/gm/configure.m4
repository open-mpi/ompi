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


# MCA_mpool_gm_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_mpool_gm_CONFIG],[
    OMPI_CHECK_GM([mpool_gm],
                     [mpool_gm_happy="yes"],
                     [mpool_gm_happy="no"])

    AS_IF([test "$mpool_gm_happy" = "yes"],
          [mpool_gm_WRAPPER_EXTRA_LDFLAGS="$mpool_gm_LDFLAGS"
           mpool_gm_WRAPPER_EXTRA_LIBS="$mpool_gm_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build gm
    AC_SUBST([mpool_gm_CFLAGS])
    AC_SUBST([mpool_gm_CPPFLAGS])
    AC_SUBST([mpool_gm_LDFLAGS])
    AC_SUBST([mpool_gm_LIBS])
])dnl
