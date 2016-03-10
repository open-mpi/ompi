# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
# Copyright (c) 2013-2016 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_CHECK_CMA(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if cma support is wanted.
AC_DEFUN([OPAL_CHECK_CMA],[
    if test -z "$ompi_check_cma_happy" ; then
	OPAL_VAR_SCOPE_PUSH([ompi_check_cma_need_defs ompi_check_cma_kernel_version])

	ompi_check_cma_happy="no"
	AC_ARG_WITH([cma],
                    [AC_HELP_STRING([--with-cma],
                                    [Build Cross Memory Attach support (default: no)])])

	# Enable CMA support by default if process_vm_readv is defined in glibc
	AC_CHECK_FUNC(process_vm_readv, [ompi_check_cma_need_defs=0],
		      [ompi_check_cma_need_defs=1])
	# If the user specifically requests CMA go ahead and enable it even
	# if the glibc version does not support process_vm_readv
	if test $ompi_check_cma_need_defs = 0 || test "x$with_cma" = "xyes" ; then
            ompi_check_cma_happy="yes"
            AC_DEFINE_UNQUOTED([OPAL_CMA_NEED_SYSCALL_DEFS],
                [$ompi_check_cma_need_defs],
                [Need CMA syscalls defined])
            AC_CHECK_HEADERS([sys/prctl.h])
	fi

	OPAL_VAR_SCOPE_POP

	OMPI_SUMMARY_ADD([[Transports]],[[Shared memory/Linux CMA]],[$1],[$ompi_check_cma_happy])
    fi

    AS_IF([test "$ompi_check_cma_happy" = "yes"], [$2], [$3])
])
