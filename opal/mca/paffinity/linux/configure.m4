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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_linux_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_linux_CONFIG],[
    # check for sched_setaffinity(), which originated in Linux kernel
    # 2.5.8, but was back-ported in several distro's 2.4 kernels.
    AC_CHECK_FUNC([sched_setaffinity],
                  [$1],
                  [$2])

    #####################################################################
    # See lengthy comment in paffinity_linux.h for an explanation of
    # these tests.
    #####################################################################

    AH_TEMPLATE([HAVE_cpu_set_t], [Whether we have the cpu_set_t type or not])
    AC_CHECK_TYPES([cpu_set_t],
                   [AC_DEFINE([HAVE_cpu_set_t], [1]) 
                    opal_paffinity_linux_have_cpu_set_t=1],
                   [opal_paffinity_linux_have_cpu_set_t=0],
                   [#include <sched.h>])

    if test "$opal_paffinity_linux_have_cpu_set_t" = "1"; then

        # Check to see if CPU_ZERO is functional (see comment in
        # paffinity_linux.h)

        AC_MSG_CHECKING([whether CPU_ZERO is broken])
        AH_TEMPLATE([HAVE_CPU_ZERO], [Whether we have a functional CPU_ZERO macro or not])
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sched.h>],
                                           [cpu_set_t foo; CPU_ZERO(&foo);])],
                           [AC_DEFINE(HAVE_CPU_ZERO, 1)
                            AC_MSG_RESULT([no])], [AC_MSG_RESULT([yes])])

        # Now check whether sched_setaffinity() takes 2 or 3
        # arguments (see comment in paffinity_linux.h)

        AC_MSG_CHECKING([how many parameters sched_setaffinity() takes])
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sched.h>],
                                           [cpu_set_t foo; sched_setaffinity(0, sizeof(foo), &foo);])],
                          [opal_paffinity_linux_ssnp=3],
                          [opal_paffinity_linux_ssnp=2])
        AC_DEFINE_UNQUOTED(OPAL_PAFFINITY_LINUX_SCHED_SETAFF_NUM_PARAMS, 
                           $opal_paffinity_linux_ssnp,
                           [How many parameters sched_setaffinity() takes (!)])
        AC_MSG_RESULT([$opal_paffinity_linux_ssnp])
    fi

    # Clean up (not entirely necessary; just being social)

    unset opal_paffinity_linux_have_cpu_set_t
    unset opal_paffinity_linux_ssnp
])dnl
