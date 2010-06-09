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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_common_sm_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_common_sm_POST_CONFIG], [
    AM_CONDITIONAL([MCA_common_sm_windows],
                   [test $1 -eq 1 -a "x$MCA_common_sm_windows" = "x1"])
    AM_CONDITIONAL([MCA_common_sm_sysv],
                   [test $1 -eq 1 -a "x$MCA_common_sm_sysv" = "x1"])
])dnl

# MCA_common_sm_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_common_sm_CONFIG], [
    OMPI_VAR_SCOPE_PUSH([MCA_common_sm_windows MCA_common_sm_sysv])

    # Are we building on Windows?
    AC_CHECK_FUNC(CreateFileMapping, 
                  [MCA_common_sm_windows=1],
                  [MCA_common_sm_windows=0])
    AC_DEFINE_UNQUOTED([MCA_COMMON_SM_WINDOWS], 
                       [$MCA_common_sm_windows],
                       [Whether we have shared memory support for Windows or not])

    # do we have sysv shared memory support on this system?
    AC_CHECK_FUNC(shmget,
                  [ompi_check_sysv_happy="yes"],
                  [ompi_check_sysv_happy="no"])

    # do we want to enable System V shared memory support?
    AC_MSG_CHECKING([if want sysv support])
    AC_ARG_ENABLE(sysv,
        AC_HELP_STRING([--enable-sysv],
                       [enable sysv shared memory support (default: disabled)]))
    if test "$enable_sysv" = "yes"; then
        if test "$ompi_check_sysv_happy" = "yes"; then
            AC_MSG_RESULT([yes])
            MCA_common_sm_sysv=1
        else
            MCA_common_sm_sysv=0
            AC_MSG_ERROR([sysv support requested but not found. aborting])
        fi
    else
        AC_MSG_RESULT([no])
        MCA_common_sm_sysv=0
    fi
    AC_DEFINE_UNQUOTED([MCA_COMMON_SM_SYSV],
                       [$MCA_common_sm_sysv],
                       [Whether we have shared memory support for SYSV or not])

])dnl

