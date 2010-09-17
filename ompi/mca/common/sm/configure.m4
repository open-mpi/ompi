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
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
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
AC_DEFUN([MCA_ompi_common_sm_POST_CONFIG], [
    AM_CONDITIONAL([COMMON_SM_BUILD_WINDOWS],
                   [test $1 -eq 1 -a "x$common_sm_build_windows" = "x1"])
    AM_CONDITIONAL([COMMON_SM_BUILD_SYSV],
                   [test $1 -eq 1 -a "x$common_sm_build_sysv" = "x1"])
    AM_CONDITIONAL([COMMON_SM_BUILD_POSIX],
                   [test $1 -eq 1 -a "x$common_sm_build_posix" = "x1"])
])dnl

# MCA_common_sm_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_common_sm_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/common/sm/Makefile])

    # Are we building on Windows?
    AC_CHECK_FUNC(CreateFileMapping, 
                  [common_sm_build_windows=1],
                  [common_sm_build_windows=0])
    AC_DEFINE_UNQUOTED([MCA_COMMON_SM_WINDOWS],
                       [$common_sm_build_windows],
                       [Whether we have shared memory support for Windows or not])

    # do we want to enable System V shared memory support?
    AC_MSG_CHECKING([if want sysv shared memory support])
    AC_ARG_ENABLE(sysv,
        AC_HELP_STRING([--disable-sysv],
                       [disable sysv shared memory support (default: enabled)]))
    AS_IF([test "$enable_sysv" = "no"],
          [AC_MSG_RESULT([no])
           common_sm_build_sysv=0],
          [AC_MSG_RESULT([yes])
           AC_CHECK_FUNC(shmget,
                  [common_sm_build_sysv=1],
                  [common_sm_build_sysv=0])])
    AS_IF([test "$enable_sysv" = "yes" -a "$common_sm_build_sysv" = "0"],
          [AC_MSG_WARN([System V shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AC_DEFINE_UNQUOTED([MCA_COMMON_SM_SYSV],
                       [$common_sm_build_sysv],
                       [Whether we have shared memory support for SYSV or not])

    # do we have the posix shm stuff?
    AC_MSG_CHECKING([if want POSIX shared memory support])
    AC_ARG_ENABLE(posix-shmem,
        AC_HELP_STRING([--disable-posix-shmem],
                       [disable posix shared memory support (default: enabled)]))
    AS_IF([test "$enable_posix_shmem" = "no"],
          [AC_MSG_RESULT([no])
           common_sm_build_posix=0],
          [AC_MSG_RESULT([yes])
           AC_SEARCH_LIBS([shm_open], [rt],
                  [common_sm_build_posix=1],
                  [common_sm_build_posix=0])])
    AS_IF([test "$enable_posix_shmem" = "yes" -a "$common_sm_build_posix" = "0"],
          [AC_MSG_WARN([POSIX shared memory support requested but not found])
           AC_MSG_ERROR([Cannot continue])])

    AC_DEFINE_UNQUOTED([MCA_COMMON_SM_POSIX],
                       [$common_sm_build_posix],
                       [Whether we have shared memory support for POSIX or not])
])dnl

