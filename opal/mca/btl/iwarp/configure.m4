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
# Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008-2011 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2011      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2018      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_iwarp_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_opal_btl_iwarp_POST_CONFIG], [
    AM_CONDITIONAL([MCA_btl_iwarp_have_rdmacm], [test $1 -eq 1 && test "x$btl_iwarp_have_rdmacm" = "x1"])
    AM_CONDITIONAL([MCA_btl_iwarp_have_dynamic_sl], [test $1 -eq 1 && test "x$btl_iwarp_have_opensm_devel" = "x1"])
])


# MCA_btl_iwarp_CONFIG([action-if-can-copalle],
#                      [action-if-cant-copalle])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_iwarp_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/iwarp/Makefile])

    OPAL_VAR_SCOPE_PUSH([cpcs btl_iwarp_LDFLAGS_save btl_iwarp_LIBS_save])
    cpcs="oob"

    OPAL_CHECK_OPENFABRICS([btl_iwarp],
                     [btl_iwarp_happy="yes"
                      OPAL_CHECK_OPENFABRICS_CM([btl_iwarp])],
                     [btl_iwarp_happy="no"])
    OPAL_CHECK_EXP_VERBS([btl_iwarp], [], [])

    AS_IF([test "$btl_iwarp_happy" = "yes"],
          [# With the new openib flags, look for ibv_openib_init
           btl_iwarp_LDFLAGS_save="$LDFLAGS"
           btl_iwarp_LIBS_save="$LIBS"
           LDFLAGS="$LDFLAGS $btl_iwarp_LDFLAGS"
           LIBS="$LIBS $btl_iwarp_LIBS"
           AC_CHECK_FUNCS([ibv_fork_init])
           LDFLAGS="$btl_iwarp_LDFLAGS_save"
           LIBS="$btl_iwarp_LIBS_save"
           $1],
          [$2])

    # substitute in the things needed to build openib
    AC_SUBST([btl_iwarp_CFLAGS])
    AC_SUBST([btl_iwarp_CPPFLAGS])
    AC_SUBST([btl_iwarp_LDFLAGS])
    AC_SUBST([btl_iwarp_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
