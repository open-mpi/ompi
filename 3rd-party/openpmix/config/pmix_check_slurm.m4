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

# PMIX_CHECK_SLURM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PMIX_CHECK_SLURM],[
    if test -z "$pmix_check_slurm_happy" ; then
	AC_ARG_WITH([slurm],
           [AS_HELP_STRING([--with-slurm],
                           [Build SLURM scheduler component (default: yes)])])

	if test "$with_slurm" = "no" ; then
            pmix_check_slurm_happy="no"
	elif test "$with_slurm" = "" ; then
            # unless user asked, only build slurm component on linux, AIX,
            # and OS X systems (these are the platforms that SLURM
            # supports)
            case $host in
		*-linux*|*-aix*|*-apple-darwin*)
                    pmix_check_slurm_happy="yes"
                    ;;
		*)
                    AC_MSG_CHECKING([for SLURM srun in PATH])
		    PMIX_WHICH([srun], [PMIX_CHECK_SLURM_SRUN])
                    if test "$PMIX_CHECK_SLURM_SRUN" = ""; then
			pmix_check_slurm_happy="no"
                    else
			pmix_check_slurm_happy="yes"
                    fi
                    AC_MSG_RESULT([$pmix_check_slurm_happy])
                    ;;
            esac
        else
            pmix_check_slurm_happy="yes"
        fi

        AS_IF([test "$pmix_check_slurm_happy" = "yes"],
              [AC_CHECK_FUNC([fork],
                             [pmix_check_slurm_happy="yes"],
                             [pmix_check_slurm_happy="no"])])

        AS_IF([test "$pmix_check_slurm_happy" = "yes"],
              [AC_CHECK_FUNC([execve],
                             [pmix_check_slurm_happy="yes"],
                             [pmix_check_slurm_happy="no"])])

        AS_IF([test "$pmix_check_slurm_happy" = "yes"],
              [AC_CHECK_FUNC([setpgid],
                             [pmix_check_slurm_happy="yes"],
                             [pmix_check_slurm_happy="no"])])

        # check to see if this is a Cray nativized slurm env.

        slurm_cray_env=0
        PMIX_CHECK_ALPS([pmix_slurm_cray],
                        [slurm_cray_env=1])

        AC_DEFINE_UNQUOTED([SLURM_CRAY_ENV],[$slurm_cray_env],
                           [defined to 1 if slurm cray env, 0 otherwise])

        PMIX_SUMMARY_ADD([Resource Managers], [Slurm], [], [$pmix_check_slurm_happy])
    fi

    AS_IF([test "$pmix_check_slurm_happy" = "yes"],
          [$2],
          [$3])
])
