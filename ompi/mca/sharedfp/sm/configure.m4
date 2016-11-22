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
# Copyright (c) 2004-2012 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008-2015 University of Houston. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_sharedfp_sm_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_sharedfp_sm_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/sharedfp/sm/Makefile])

    sharedfp_sm_happy=no
    AC_CHECK_HEADER([semaphore.h],
                    [AC_CHECK_FUNCS([sem_open],[sharedfp_sm_happy=yes],[])])

    AC_CHECK_HEADER([semaphore.h],
        [AC_CHECK_FUNCS([sem_init],[sharedfp_sm_happy=yes],[])])

    AS_IF([test "$sharedfp_sm_happy" = "yes"],
          [$1],
          [$2])
])dnl
