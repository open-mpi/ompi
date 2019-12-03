# -*- shell-script -*-
#
#
# Copyright (c) 2011 Mellanox Technologies. All rights reserved.
# Copyright (c) 2019 Huawei Technologies Co., Ltd. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_coll_ucx_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([COLL])])
])

# MCA_coll_ucx_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_ucx_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/ucx/Makefile])

    OMPI_CHECK_UCX([coll_ucx],
                     [coll_ucx_happy="yes"],
                     [coll_ucx_happy="no"])

    AS_IF([test "$coll_ucx_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ucx
    AC_SUBST([coll_ucx_CFLAGS])
    AC_SUBST([coll_ucx_CPPFLAGS])
    AC_SUBST([coll_ucx_LDFLAGS])
    AC_SUBST([coll_ucx_LIBS])
])dnl

