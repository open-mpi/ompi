dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2014-2016 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
dnl Copyright (c) 2019      Triad National Security, LLC. All rights.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl OPAL_CONFIG_QTHREADS()
dnl
dnl Configure argobot threads, setting the following variables (but
dnl  not calling AC_SUBST on them).

#********************************************************************
#
# TODO: undoubtedly need some better check than this
#
#********************************************************************
AC_DEFUN([OPAL_CONFIG_QTHREADS],[

    AC_CHECK_HEADERS([mach/mach_time.h],
                     [AC_CHECK_FUNC([mach_absolute_time],
                                    [threads_qthreads_happy="yes"],
                                    [threads_qthreads_happy="no"])],
                     [threads_qthreads_happy="no"])

    AS_IF([test "$threads_qthreads_happy" = "yes"],
          [$1],
          [$2])
])dnl
