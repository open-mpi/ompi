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
	OPAL_VAR_SCOPE_PUSH([ompi_check_cma_need_defs ompi_check_cma_kernel_version ompi_check_cma_CFLAGS])

	AC_ARG_WITH([cma],
                    [AC_HELP_STRING([--with-cma],
                                    [Build Cross Memory Attach support (default: autodetect)])])

	# Enable CMA support by default if process_vm_readv is defined in glibc
	AC_CHECK_FUNC(process_vm_readv, [ompi_check_cma_need_defs=0],
		      [ompi_check_cma_need_defs=1])

	if test $ompi_check_cma_need_defs = 1 ; then
	    ompi_check_cma_CFLAGS="$CFLAGS"
	    # Need some extra include paths to locate the appropriate headers
	    CFLAGS="$CFLAGS -I${srcdir} -I${srcdir}/opal/include"
	    AC_MSG_CHECKING([if internal syscall numbers for Linux CMA work])
	    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include <sys/uio.h>
#include <sys/wait.h>
#include <sys/syscall.h>

#include "opal/include/opal/sys/cma.h"

static void do_check (pid_t pid, int *in, int *out)
{
    int check[4] = {0, 0, 0, 0}, i;
    struct iovec rem_iov = {out, sizeof (check)};
    struct iovec loc_iov = {check, sizeof (check)};
    ssize_t rc;

    rc = process_vm_readv (pid, &loc_iov, 1, &rem_iov, 1, 0);
    if (sizeof (check) != rc) {
        exit (1);
    }

    for (i = 0 ; i < 4 ; ++i) {
        if (check[i] != i) {
            exit (1);
        }

        check[i] = i * 2;
    }

    rem_iov.iov_base = in;
    rc = process_vm_writev (pid, &loc_iov, 1, &rem_iov, 1, 0);
    if (sizeof (check) != rc) {
        exit (1);
    }

    exit (0);
}
]],[[
    int i, in[4] = {-1, -1, -1, -1}, out[4] = {0, 1, 2, 3};

    do_check (getpid (), in, out);

    for (i = 0 ; i < 4 ; ++i) {
        if (in[i] != 2 * i) {
            return 1;
        }
    }

    /* all good */
    return 0;
]])],
			  [AC_MSG_RESULT([yes])
			   ompi_check_cma_happy="yes"],
			  [AC_MSG_RESULT([no])
			   ompi_check_cma_happy="no"],
			  [AC_MSG_RESULT([no (cross-compiling)])
			   ompi_check_cma_happy="no"])
	    CFLAGS="$ompi_check_cma_CFLAGS"
	else
	    ompi_check_cma_happy="yes"
	fi

	# If the user specifically requests CMA go ahead and enable it even
	# if the glibc version does not support process_vm_readv
	if test "x$with_cma" = "xyes" || test "$ompi_check_cma_happy" = "yes" ; then
            ompi_check_cma_happy="yes"
            AC_DEFINE_UNQUOTED([OPAL_CMA_NEED_SYSCALL_DEFS],
                [$ompi_check_cma_need_defs],
                [Need CMA syscalls defined])
            AC_CHECK_HEADERS([sys/prctl.h])
	fi

	OPAL_VAR_SCOPE_POP

	OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/Linux CMA]],[$1],[$ompi_check_cma_happy])
    fi

    AS_IF([test "$ompi_check_cma_happy" = "yes"], [$2], [$3])
])
