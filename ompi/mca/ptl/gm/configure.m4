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


# MCA_ptl_gm_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ptl_gm_CONFIG],[
    OMPI_CHECK_GM([ptl_gm],
                     [ptl_gm_happy="yes"],
                     [ptl_gm_happy="no"])

    AS_IF([test "$ptl_gm_happy" = "yes"],
          [ptl_gm_WRAPPER_EXTRA_LDFLAGS="$ptl_gm_LDFLAGS"
           ptl_gm_WRAPPER_EXTRA_LIBS="$ptl_gm_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build gm
    AC_SUBST([ptl_gm_CFLAGS])
    AC_SUBST([ptl_gm_CPPFLAGS])
    AC_SUBST([ptl_gm_LDFLAGS])
    AC_SUBST([ptl_gm_LIBS])
    # Define it for internal use.
    AC_DEFINE_UNQUOTED(OMPI_MCA_PTL_GM_CACHE_ENABLE, 0,
            [Whether we want the internal GM cache to be activated.])
])dnl
