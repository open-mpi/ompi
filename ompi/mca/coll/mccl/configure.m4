# -*- shell-script -*-
#
#
# Copyright (c) 2020      Mellanox Technologies. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_coll_mccl_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_mccl_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/mccl/Makefile])

    OMPI_CHECK_MCCL([coll_mccl],
                     [coll_mccl_happy="yes"],
                     [coll_mccl_happy="no"])

    AS_IF([test "$coll_mccl_happy" = "yes"],
          [coll_mccl_WRAPPER_EXTRA_LDFLAGS="$coll_mccl_LDFLAGS"
           coll_mccl_CPPFLAGS="$coll_mccl_CPPFLAGS"
           coll_mccl_WRAPPER_EXTRA_LIBS="$coll_mccl_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build mccl
    AC_SUBST([coll_mccl_CFLAGS])
    AC_SUBST([coll_mccl_CPPFLAGS])
    AC_SUBST([coll_mccl_LDFLAGS])
    AC_SUBST([coll_mccl_LIBS])
])dnl

