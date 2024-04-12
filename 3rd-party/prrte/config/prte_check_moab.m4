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
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# PRTE_CHECK_MOAB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PRTE_CHECK_MOAB],[
        PRTE_VAR_SCOPE_PUSH([prte_check_moab_$1_save_CPPFLAGS prte_check_moab_$1_save_LDFLAGS prte_check_moab_$1_save_LIBS])

        AC_ARG_WITH([moab],
                    [AS_HELP_STRING([--with-moab],
                                    [Build MOAB scheduler component (default: yes)])])
        AC_ARG_WITH([moab-libdir],
                    [AS_HELP_STRING([--with-moab-libdir=DIR],
                    [Search for Moab libraries in DIR])])

        prte_check_moab_happy="yes"
        AS_IF([test "$with_moab" = "no"],
              [prte_check_moab_happy=no])

        prte_check_moab_$1_save_CPPFLAGS=$CPPFLAGS
        prte_check_moab_$1_save_LDFLAGS=$LDFLAGS
        prte_check_moab_$1_save_LIBS=$LIBS

        AS_IF([test $prte_check_moab_happy = yes],
              [OAC_CHECK_PACKAGE([moab],
                                 [prte_check_moab],
                                 [mapi.h],
                                 [cmoab],
                                 [MCCJobGetRemainingTime],
                                 [],
                                 [prte_check_moab_happy=no])])

        CPPFLAGS=$prte_check_moab_$1_save_CPPFLAGS
        LDFLAGS=$prte_check_moab_$1_save_LDFLAGS
        LIBS=$prte_check_moab_$1_save_LIBS

        PRTE_SUMMARY_ADD([Resource Managers], [Moab], [], [$prte_check_moab_SUMMARY])
        PRTE_VAR_SCOPE_POP

    if test $prte_check_moab_happy = yes ; then
        $1_CPPFLAGS="[$]$1_CPPFLAGS $prte_check_moab_CPPFLAGS"
        $1_LIBS="[$]$1_LIBS $prte_check_moab_LIBS"
        $1_LDFLAGS="[$]$1_LDFLAGS $prte_check_moab_LDFLAGS"

        AC_SUBST($1_CPPFLAGS)
        AC_SUBST($1_LDFLAGS)
        AC_SUBST($1_LIBS)
    fi

    AS_IF([test "$prte_check_moab_happy" = "yes"],
          [$2],
          [$3])
])
