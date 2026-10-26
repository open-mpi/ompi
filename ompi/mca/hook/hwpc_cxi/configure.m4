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
    AC_CONFIG_FILES([ompi/mca/hook/hwpc_cxi/test-multi/Makefile])
    AC_MSG_CHECKING([if want hardware-based performance counters for Cassini (HWPC_CXI)])
    AC_ARG_ENABLE([hwpc-cxi],
        [AS_HELP_STRING([--enable-hwpc-cxi],
        [Enable hardware-based performance counters capability for HPE's Cassini devices (default: enabled; built as a no-op if libcxi is not found; an explicit --enable-hwpc-cxi aborts configure if it is not found)])],
        [enable_hwpc_cxi=$(echo "$enableval" | tr 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 'abcdefghijklmnopqrstuvwxyz')],
        [enable_hwpc_cxi=default])
    AS_CASE([$enable_hwpc_cxi],
            [yes|y|true|1], [enable_hwpc_cxi=yes],
            [no|n|false|0], [enable_hwpc_cxi=no],
            [default], [],
            [AC_MSG_ERROR([--enable-hwpc-cxi expects yes|y|true|1 or no|n|false|0])])
    AC_MSG_RESULT([$enable_hwpc_cxi])

    AC_ARG_WITH([libcxi],
        [AS_HELP_STRING([--with-libcxi(=DIR)],
        [Search for libcxi (used to detect a Cassini build environment for HWPC-CXI), optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path. Without DIR, the default search paths and then /usr/local are tried.])])

    HWPC_CXI_ENABLE=0
    ompi_hook_hwpc_cxi_have_libcxi=0
    ompi_hook_hwpc_cxi_summary="no (disabled)"
    AS_IF([test "$enable_hwpc_cxi" != "no"], [
        OAC_CHECK_PACKAGE([libcxi],
                          [hook_hwpc_cxi],
                          [libcxi/libcxi.h],
                          [cxi],
                          [cxil_get_device_list],
                          [ompi_hook_hwpc_cxi_have_libcxi=1],
                          [ompi_hook_hwpc_cxi_have_libcxi=0])
        AS_IF([test "$ompi_hook_hwpc_cxi_have_libcxi" = "0" && test -z "$with_libcxi"], [
            with_libcxi=/usr/local
            AS_UNSET([ac_cv_header_libcxi_libcxi_h])
            AS_UNSET([ac_cv_func_cxil_get_device_list])
            OAC_CHECK_PACKAGE([libcxi],
                              [hook_hwpc_cxi],
                              [libcxi/libcxi.h],
                              [cxi],
                              [cxil_get_device_list],
                              [ompi_hook_hwpc_cxi_have_libcxi=1],
                              [ompi_hook_hwpc_cxi_have_libcxi=0])
            AS_UNSET([with_libcxi])
        ])
        AS_IF([test "$ompi_hook_hwpc_cxi_have_libcxi" = "0"],
              [AS_IF([test "$enable_hwpc_cxi" = "yes" || (test -n "$with_libcxi" && test "$with_libcxi" != "no")],
                     [AC_MSG_ERROR([HWPC-CXI support was requested, but libcxi was not found.  Use --with-libcxi=DIR to point at it.])])])
        $1
        AS_IF([test "$should_build" = "1"], [
            HWPC_CXI_ENABLE=1
            AS_IF([test "$ompi_hook_hwpc_cxi_have_libcxi" = "1"],
                  [ompi_hook_hwpc_cxi_summary="yes"],
                  [ompi_hook_hwpc_cxi_summary="no-op (Cassini headers not found)"
                   AC_MSG_WARN([libcxi not found; HWPC-CXI will be built as a no-op])])
        ])
    ], [
        $2
    ])
    AS_IF([test "$ompi_hook_hwpc_cxi_have_libcxi" = "0"], [hook_hwpc_cxi_CPPFLAGS=])
    AC_SUBST([hook_hwpc_cxi_CPPFLAGS])
    AC_DEFINE_UNQUOTED([OMPI_HOOK_HWPC_CXI_HAVE_LIBCXI], [$ompi_hook_hwpc_cxi_have_libcxi],
                       [Whether libcxi was found, enabling HWPC-CXI counter collection])
    AM_CONDITIONAL([HWPC_CXI_ENABLE], [test "$HWPC_CXI_ENABLE" = "1"])
    OPAL_SUMMARY_ADD([Miscellaneous], [HWPC-CXI support], [], [$ompi_hook_hwpc_cxi_summary])
])
