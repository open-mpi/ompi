# -*- shell-script -*-
#
#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_oshmem_scoll_fca_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_scoll_fca_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/scoll/fca/Makefile])

    OMPI_CHECK_FCA([scoll_fca],
                     [scoll_fca_happy="yes"],
                     [scoll_fca_happy="no"])

    AS_IF([test "$scoll_fca_happy" = "yes"],
          [scoll_fca_WRAPPER_EXTRA_LDFLAGS="$scoll_fca_LDFLAGS"
           scoll_fca_CPPFLAGS="$scoll_fca_CPPFLAGS"
           scoll_fca_WRAPPER_EXTRA_CPPFLAGS="$scoll_fca_CPPFLAGS"
           scoll_fca_WRAPPER_EXTRA_LIBS="$scoll_fca_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build fca
    AC_SUBST([scoll_fca_CFLAGS])
    AC_SUBST([scoll_fca_CPPFLAGS])
    AC_SUBST([scoll_fca_LDFLAGS])
    AC_SUBST([scoll_fca_LIBS])
	AC_SUBST(scoll_fca_HOME, "$ompi_check_fca_dir")
])dnl

