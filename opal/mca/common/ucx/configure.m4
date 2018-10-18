# -*- shell-script -*-
#
# Copyright (c) 2018      Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_common_sm_CONFIG([action-if-can-compile],
#                           [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_common_ucx_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/ucx/Makefile])
    common_ucx_happy="no"
    OMPI_CHECK_UCX([common_ucx],
               [common_ucx_happy="yes"],
               [common_ucx_happy="no"])

    AS_IF([test "$common_ucx_happy" = "yes"],
          [$1],
          [$2])


    # substitute in the things needed to build common_ucx
    AC_SUBST([common_ucx_CFLAGS])
    AC_SUBST([common_ucx_CPPFLAGS])
    AC_SUBST([common_ucx_LDFLAGS])
    AC_SUBST([common_ucx_LIBS])
])dnl


