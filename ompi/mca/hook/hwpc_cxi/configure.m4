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
        [Enable hardware-based performance counters capability for HPE's Cassini devices (default: disabled)])],
        [enable_hwpc_cxi=$(echo "$enableval" | tr '[:upper:]' '[:lower:]')],
        [enable_hwpc_cxi=no])
    AS_CASE([$enable_hwpc_cxi],
            [yes|y|true|1], [enable_hwpc_cxi=yes],
            [no|n|false|0], [enable_hwpc_cxi=no],
            [AC_MSG_ERROR([--enable-hwpc-cxi expects yes|y|true|1 or no|n|false|0])])
    AC_MSG_RESULT([$enable_hwpc_cxi])
    ompi_hook_hwpc_cxi_happy=no
    AS_IF([test "$enable_hwpc_cxi" = "yes"], [
        AC_CHECK_HEADERS([cxi_prov_hw.h], [ompi_hook_hwpc_cxi_happy=yes])
    ])
    AS_IF([test "$enable_hwpc_cxi" = "yes" && test "$ompi_hook_hwpc_cxi_happy" != "yes"], [
        AC_MSG_WARN([--enable-hwpc-cxi was requested, but cxi_prov_hw.h was not found; disabling hwpc_cxi support])
    ])
    AS_IF([test "$ompi_hook_hwpc_cxi_happy" = "yes"],
          [
           HWPC_CXI_ENABLE=1
           $1
          ],
          [
           HWPC_CXI_ENABLE=0
           $2
          ])
    AC_DEFINE_UNQUOTED([HWPC_CXI_ENABLE],
                       [$HWPC_CXI_ENABLE],
                       [If the hardware-based performance counters capability for HPE's Cassini devices should be enabled.])
    AM_CONDITIONAL([HWPC_CXI_ENABLE], [test "$HWPC_CXI_ENABLE" = "1"])
    AM_CONDITIONAL([OMPI_HAVE_CXI_PROVIDER_HEADER], [test "$ompi_hook_hwpc_cxi_happy" = "yes"])
])
