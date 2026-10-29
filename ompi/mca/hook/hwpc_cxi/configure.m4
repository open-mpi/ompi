#
# SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#
# Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Make this a static component
AC_DEFUN([MCA_ompi_hook_hwpc_cxi_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

#
# MCA_hook_hwpc_cxi_CONFIG([action-if-can-compile],
#                          [action-if-cant-compile])
# ----------------------------------------------------
AC_DEFUN([MCA_ompi_hook_hwpc_cxi_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/hook/hwpc_cxi/Makefile])
    AC_CONFIG_FILES([ompi/mca/hook/hwpc_cxi/test/Makefile])
    AC_MSG_CHECKING([if want hardware-based performance counters for Cassini (HWPC_CXI)])
    AC_ARG_ENABLE([hwpc-cxi],
        [AS_HELP_STRING([--enable-hwpc-cxi],
        [Enable hardware-based performance counters capability for HPE's Cassini devices (default: enabled; counters are only collected on nodes with a Cassini device)])],
        [enable_hwpc_cxi=$(echo "$enableval" | tr 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 'abcdefghijklmnopqrstuvwxyz')],
        [enable_hwpc_cxi=yes])
    AS_CASE([$enable_hwpc_cxi],
            [yes|y|true|1], [enable_hwpc_cxi=yes],
            [no|n|false|0], [enable_hwpc_cxi=no],
            [AC_MSG_ERROR([--enable-hwpc-cxi expects yes|y|true|1 or no|n|false|0])])
    AC_MSG_RESULT([$enable_hwpc_cxi])

    HWPC_CXI_ENABLE=0
    ompi_hook_hwpc_cxi_summary="no (disabled)"
    AS_IF([test "$enable_hwpc_cxi" != "no"], [
        $1
        AS_IF([test "$should_build" = "1"], [
            HWPC_CXI_ENABLE=1
            ompi_hook_hwpc_cxi_summary="yes"
        ])
    ], [
        $2
    ])
    AM_CONDITIONAL([HWPC_CXI_ENABLE], [test "$HWPC_CXI_ENABLE" = "1"])
    OPAL_SUMMARY_ADD([Miscellaneous], [HWPC-CXI support], [], [$ompi_hook_hwpc_cxi_summary])
])
