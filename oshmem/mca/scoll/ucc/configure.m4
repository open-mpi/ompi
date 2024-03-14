# -*- shell-script -*-
#
#
# Copyright (c) 2021      Mellanox Technologies. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_scoll_ucc_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_scoll_ucc_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/scoll/ucc/Makefile])

    OMPI_CHECK_UCC([scoll_ucc],
                     [scoll_ucc_happy="yes"],
                     [scoll_ucc_happy="no"])

    AS_IF([test "$scoll_ucc_happy" = "yes"],
          [scoll_ucc_WRAPPER_EXTRA_LDFLAGS="$scoll_ucc_LDFLAGS"
           scoll_ucc_CPPFLAGS="$scoll_ucc_CPPFLAGS"
           scoll_ucc_WRAPPER_EXTRA_LIBS="$scoll_ucc_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build ucc
    AC_SUBST([scoll_ucc_CPPFLAGS])
    AC_SUBST([scoll_ucc_LDFLAGS])
    AC_SUBST([scoll_ucc_LIBS])
])dnl

