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
# Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_common_ofacm_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_opal_common_ofacm_POST_CONFIG], [
    AM_CONDITIONAL([MCA_common_ofacm_have_xrc], [test $1 -eq 1 -a "x$common_ofacm_have_xrc" = "x1"])
])


# MCA_opal_common_ofacm_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_common_ofacm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/ofacm/Makefile])
    OPAL_VAR_SCOPE_PUSH([modules ofacm_have_threads])
    modules="oob"

    common_ofacm_happy="no"
    OPAL_CHECK_OPENFABRICS([common_ofacm],
                           [common_ofacm_happy="yes"
                            OPAL_CHECK_OPENFABRICS_CM([common_ofacm])])

    AS_IF([test "$common_ofacm_happy" = "yes"],
          [$1],
          [$2])

    AS_IF([test "$common_ofacm_happy" = "yes"],
          [if test "x$common_ofacm_have_xrc" = "x1"; then
              modules="$modules xoob"
          fi
          AC_MSG_CHECKING([which OpenFabrics CM modules will be built])
          AC_MSG_RESULT([$modules])])

    # substitute in the things needed to build openib
    AC_SUBST([common_ofacm_CFLAGS])
    AC_SUBST([common_ofacm_CPPFLAGS])
    AC_SUBST([common_ofacm_LDFLAGS])
    AC_SUBST([common_ofacm_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
