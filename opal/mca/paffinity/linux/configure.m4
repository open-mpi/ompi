# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ptl_tcp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_linux_CONFIG],[
    # check for sched_setaffinity(), which originated in Linux kernel
    # 2.5.8, but was back-ported in several distro's 2.4 kernels.
    AC_CHECK_FUNC([sched_setaffinity],
                  [$1],
                  [$2])

    # Linux sucks.  There are at least 3 different ways that
    # sched_setaffinity is implemented (only one of which -- the most
    # outdated -- is documented in the sched_setaffinity(2) man page).

    # 1. int sched_setaffinity(pid_t pid, unsigned int len, unsigned
    # long *mask);

    # This originated in 2.5 kernels (which we won't worry about) and
    # some distros back-ported it to their 2.4 kernels.  It's unknown
    # if this appears in any 2.6 kernels.

    # 2. int sched_setaffinity (pid_t __pid, size_t __cpusetsize,
    # const cpu_set_t *__cpuset);

    # This appears to be in recent 2.6 kernels (e.g., 2.6.11).  I
    # don't know when #1 changed into #2.  However, this prototype is
    # the nicest -- the cpu_set_t type is accompanied by fdset-like
    # CPU_ZERO(), CPU_SET(), CPU_ISSET(), etc. macros.

    # 3. int sched_setaffinity (pid_t __pid, const cpu_set_t *__mask);

    # (note the missing len parameter) This may be an SGI Altix
    # exclusive -- they appear to have a 2.4-based kernel, and
    # therefore likely back-ported the 2.5 work but modified it for
    # their needs.  Similar to #2, the cpu_set_t type is accompanied
    # by fdset-like CPU_ZERO(), CPU_SET(), CPU_ISSET(), etc. macros.

    # This configure script has to figure out which one to use.  :-\

    AH_TEMPLATE([HAVE_cpu_set_t], [Whether we have the cpu_set_t type or not])
    AC_CHECK_TYPES([cpu_set_t],
                   [AC_DEFINE([HAVE_cpu_set_t], [1]) have_cpu_set_t=1],
                   [have_cpu_set_t=0],
                   [#include <sched.h>])

    if test "$have_cpu_set_t" = "1"; then
        # Note: SGI Altix tests not written yet -- having other
        # problems with that machine and awaiting expert help from SGI
        # engineers.
        echo > /dev/null
    fi
    unset have_cpu_set_t
])dnl
