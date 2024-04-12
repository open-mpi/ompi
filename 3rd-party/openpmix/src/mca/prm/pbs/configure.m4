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
# Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_prm_pbs_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_pmix_prm_pbs_CONFIG],[
    AC_CONFIG_FILES([src/mca/prm/pbs/Makefile])

    AC_ARG_WITH([pbs],
           [AS_HELP_STRING([--with-pbs],
                           [Build PBS scheduler component (default: yes)])])

    if test "$with_pbs" = "no" ; then
            pmix_check_pbs_happy="no"
    elif test "$with_pbs" = "" ; then
            # unless user asked, only build pbs component on linux, AIX,
            # and OS X systems (these are the platforms that PBS
            # supports)
            case $host in
        *-linux*|*-aix*|*-apple-darwin*)
                    pmix_check_pbs_happy="yes"
                    ;;
        *)
                    AC_MSG_CHECKING([for pbsdash in PATH])
                    PMIX_WHICH([pbsdash], [PMIX_CHECK_PBSDASH])
                    if test "$PMIX_CHECK_PBSDASH" = ""; then
                        pmix_check_pbs_happy="no"
                    else
                        pmix_check_pbs_happy="yes"
                    fi
                    AC_MSG_RESULT([$pmix_check_pbs_happy])
                    ;;
            esac
    else
        pmix_check_pbs_happy="yes"
    fi

    PMIX_SUMMARY_ADD([Resource Managers], [PBS], [], [$pmix_check_pbs_happy (scheduler)])

    AS_IF([test "$pmix_check_pbs_happy" = "yes"],
          [$1],
          [$2])

])dnl
