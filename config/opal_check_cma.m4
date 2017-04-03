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
    AC_ARG_WITH([cma],
                [AC_HELP_STRING([--with-cma],
                                [Build Cross Memory Attach support (default: autodetect)])])

    if test "x$with_cma" = "xno" ; then
        opal_check_cma_happy=0
    fi

    # We only need to do the back-end test once
    if test -z "$opal_check_cma_happy" ; then
        OPAL_CHECK_CMA_BACKEND
    fi

    AS_IF([test $opal_check_cma_happy -eq 1],
          [$2],
          [if test "$with_cma" = "yes"; then
               AC_MSG_WARN([--with-cma support requested, but not available])
               AC_MSG_ERROR([Cannot continue])
           fi
           $3])
])

AC_DEFUN([OPAL_CHECK_CMA_BACKEND],
[
    OPAL_VAR_SCOPE_PUSH([opal_check_cma_need_defs opal_check_cma_kernel_version opal_check_cma_CFLAGS opal_check_cma_msg])

    # Some systems have process_cm_readv() in libc, which means CMA is
    # supported.  Other systems do not have process_cm_readv() in
    # libc, but have support for it in the kernel if we invoke it
    # directly.  Check for both.
    AC_CHECK_HEADERS([sys/prctl.h])

    AC_CHECK_FUNC([process_vm_readv], [opal_check_cma_need_defs=0],
                  [opal_check_cma_need_defs=1])
    AC_DEFINE_UNQUOTED([OPAL_CMA_NEED_SYSCALL_DEFS],
                       [$opal_check_cma_need_defs],
                       [Need CMA syscalls defined])
    if test $opal_check_cma_need_defs -eq 1 ; then
        opal_check_cma_CFLAGS=$CFLAGS
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
		       opal_check_cma_happy=1],
		      [AC_MSG_RESULT([no])
		       opal_check_cma_happy=0],
		      [AC_MSG_RESULT([no (cross-compiling)])
		       opal_check_cma_happy=0])
	CFLAGS=$opal_check_cma_CFLAGS
    else
        # If we didn't need the defs, then we have process_vm_readv(),
        # and CMA is happy.
	opal_check_cma_happy=1
    fi

    OPAL_VAR_SCOPE_POP

    AS_IF([test $opal_check_cma_happy -eq 1],
          [opal_check_cma_msg=yes],
          [opal_check_cma_msg=no])
    OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/Linux CMA]],[$1],[$opal_check_cma_msg])
])
