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
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ras_pbs_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_prte_ras_pbs_CONFIG],[
    AC_CONFIG_FILES([src/mca/ras/pbs/Makefile])

	AC_ARG_WITH([pbs],
           [AS_HELP_STRING([--with-pbs],
                           [Build PBS scheduler component (default: yes)])])

	if test "$with_pbs" = "no" ; then
            prte_check_pbs_happy="no"
	elif test "$with_pbs" = "" ; then
            # unless user asked, only build pbs component on linux, AIX,
            # and OS X systems (these are the platforms that PBS
            # supports)
            case $host in
		*-linux*|*-aix*|*-apple-darwin*)
                    prte_check_pbs_happy="yes"
                    ;;
		*)
                    AC_MSG_CHECKING([for pbsdash in PATH])
                    PRTE_WHICH([pbsdash], [PRTE_CHECK_PBSDASH])
                    if test "$PRTE_CHECK_PBSDASH" = ""; then
                        prte_check_pbs_happy="no"
                    else
                        prte_check_pbs_happy="yes"
                    fi
                    AC_MSG_RESULT([$prte_check_pbs_happy])
                    ;;
            esac
    else
        prte_check_pbs_happy="yes"
    fi

    PRTE_SUMMARY_ADD([Resource Managers], [PBS], [], [$prte_check_pbs_happy (scheduler)])

    AS_IF([test "$prte_check_pbs_happy" = "yes"],
          [$1],
          [$2])

])dnl
