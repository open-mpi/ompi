dnl -*- shell-script -*-
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
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl SPDX-License-Identifier: BSD-3-Clause-Open-MPI
dnl

AC_DEFUN([MCA_opal_memory_CONFIG],[
        AC_ARG_WITH([memory-manager],
            [AS_HELP_STRING([--with-memory-manager=TYPE],
                           [Use TYPE for intercepting memory management
                            calls to control memory pinning.])])

        memory_base_found=0
        memory_base_want=1
        AS_IF([test "$with_memory_manager" = "no"], [memory_base_want=0])
        MCA_CONFIGURE_FRAMEWORK($1, $2, $memory_base_want)

        AC_DEFINE_UNQUOTED([OPAL_MEMORY_HAVE_COMPONENT], [$memory_base_found],
            [Whether any opal memory mca components were found])
])
