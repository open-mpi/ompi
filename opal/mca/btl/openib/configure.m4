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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_openib_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_opal_btl_openib_POST_CONFIG], [
    AM_CONDITIONAL([MCA_btl_openib_have_xrc], [test $1 -eq 1 && test "x$btl_openib_have_xrc" = "x1"])
    AM_CONDITIONAL([MCA_btl_openib_have_rdmacm], [test $1 -eq 1 && test "x$btl_openib_have_rdmacm" = "x1"])
    AM_CONDITIONAL([MCA_btl_openib_have_dynamic_sl], [test $1 -eq 1 && test "x$btl_openib_have_opensm_devel" = "x1"])
    AM_CONDITIONAL([MCA_btl_openib_have_udcm], [test $1 -eq 1 && test "x$btl_openib_have_udcm" = "x1"])
])


# MCA_btl_openib_CONFIG([action-if-can-copalle],
#                      [action-if-cant-copalle])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_openib_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/openib/Makefile])

    OPAL_VAR_SCOPE_PUSH([cpcs LDFLAGS_save LIBS_save])
    cpcs="oob"

    OPAL_CHECK_OPENFABRICS([btl_openib],
                     [btl_openib_happy="yes"
                      OPAL_CHECK_OPENFABRICS_CM([btl_openib])],
                     [btl_openib_happy="no"])
    OPAL_CHECK_EXP_VERBS([btl_openib], [], [])

    AS_IF([test "$btl_openib_happy" = "yes"],
          [# With the new openib flags, look for ibv_fork_init
           LDFLAGS_save="$LDFLAGS"
           LIBS_save="$LIBS"
           LDFLAGS="$LDFLAGS $btl_openib_LDFLAGS"
           LIBS="$LIBS $btl_openib_LIBS"
           AC_CHECK_FUNCS([ibv_fork_init])
           LDFLAGS="$LDFLAGS_save"
           LIBS="$LIBS_save"
           $1],
          [$2])

    AS_IF([test "$btl_openib_happy" = "yes"],
          [if test "x$btl_openib_have_xrc" = "x1"; then
              cpcs="$cpcs xoob"
          fi
          if test "x$btl_openib_have_rdmacm" = "x1"; then
              cpcs="$cpcs rdmacm"
              if test "$enable_openib_rdmacm_ibaddr" = "yes"; then
                  AC_MSG_CHECKING([IB addressing])
                  AC_EGREP_CPP(
                      yes,
                      [
                        #include <infiniband/ib.h>
                        #ifdef AF_IB
                          yes
                        #endif
                      ],
                      [
                        AC_CHECK_HEADERS(
                            [rdma/rsocket.h],
                            [
                                AC_MSG_RESULT([yes])
                                AC_DEFINE(BTL_OPENIB_RDMACM_IB_ADDR, 1, rdmacm IB_AF addressing support)
                            ],
                            [
                                AC_MSG_RESULT([no])
                                AC_DEFINE(BTL_OPENIB_RDMACM_IB_ADDR, 0, rdmacm without IB_AF addressing support)
                                AC_MSG_WARN([There is no IB_AF addressing support by lib rdmacm.])
                            ]
                      )],
                      [
                       AC_MSG_RESULT([no])
                       AC_DEFINE(BTL_OPENIB_RDMACM_IB_ADDR, 0, rdmacm without IB_AF addressing support)
                       AC_MSG_WARN([There is no IB_AF addressing support by lib rdmacm.])
                      ])
              else
                AC_DEFINE(BTL_OPENIB_RDMACM_IB_ADDR, 0, rdmacm without IB_AF addressing support)
              fi
          fi
          if test "x$btl_openib_have_udcm" = "x1"; then
              cpcs="$cpcs udcm"
          fi
          AC_MSG_CHECKING([which openib btl cpcs will be built])
          AC_MSG_RESULT([$cpcs])])

    # Enable openib device failover.  It is disabled by default.
    AC_MSG_CHECKING([whether openib failover is enabled])
    AC_ARG_ENABLE([btl-openib-failover],
       [AC_HELP_STRING([--enable-btl-openib-failover],
           [enable openib BTL failover (default: disabled)])])
    if test "$enable_btl_openib_failover" = "yes"; then
        AC_MSG_RESULT([yes])
        btl_openib_failover_enabled=1
    else
        AC_MSG_RESULT([no])
        btl_openib_failover_enabled=0
    fi
    AC_DEFINE_UNQUOTED([BTL_OPENIB_FAILOVER_ENABLED], [$btl_openib_failover_enabled],
                       [enable openib BTL failover])
    AM_CONDITIONAL([MCA_btl_openib_enable_failover], [test "x$btl_openib_failover_enabled" = "x1"])

    # make sure that CUDA-aware checks have been done
    AC_REQUIRE([OPAL_CHECK_CUDA])

    # substitute in the things needed to build openib
    AC_SUBST([btl_openib_CFLAGS])
    AC_SUBST([btl_openib_CPPFLAGS])
    AC_SUBST([btl_openib_LDFLAGS])
    AC_SUBST([btl_openib_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
