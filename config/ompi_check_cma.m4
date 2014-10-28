# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
# Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_CMA(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if cma support is wanted.
AC_DEFUN([OMPI_CHECK_CMA],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_cma_happy ompi_check_cma_need_defs])

    ompi_check_cma_happy="no"
    AC_ARG_WITH([cma],
                [AC_HELP_STRING([--with-cma],
                                [Build Cross Memory Attach support (default: no)])])

    AC_MSG_CHECKING([if user requested CMA build])
    if test "$with_cma" = "yes" ; then
            ompi_check_cma_happy="yes"
            AC_MSG_RESULT([yes])
            AC_CHECK_FUNC(process_vm_readv, [ompi_check_cma_need_defs=0],
                [ompi_check_cma_need_defs=1])
            AC_DEFINE_UNQUOTED([OMPI_CMA_NEED_SYSCALL_DEFS],
                [$ompi_check_cma_need_defs],
                [Need CMA syscalls defined])
            AC_CHECK_HEADERS([sys/prctl.h])
    else
        AC_MSG_RESULT([no])
    fi
    AS_IF([test "$ompi_check_cma_happy" = "yes"],
            [$2],
            [$3])

    OPAL_VAR_SCOPE_POP
])dnl
