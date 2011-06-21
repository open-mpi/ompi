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
# Copyright (c) 2010-2011 Los Alamos National Security, LLC.
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
])dnl

# MCA_ompi_common_sm_CONFIG([action-if-can-compile],
#                           [action-if-cant-compile])
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
])dnl

