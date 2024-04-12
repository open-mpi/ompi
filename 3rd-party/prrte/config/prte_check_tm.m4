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
dnl Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# PRTE_CHECK_TM_PBS_CONFIG_RUN([pbs-config args], [assigmnent variable],
#                              [action-if-successful], [action-if-not-successful])
# --------------------------------------------------------------------------------
AC_DEFUN([PRTE_CHECK_TM_PBS_CONFIG_RUN], [
    # bozo check
    AS_IF([test -z "${prte_check_tm_cv_pbs_config_path}"],
          [AC_MSG_ERROR([Internal error.  pbs-config not properly configured.])])

    PRTE_LOG_COMMAND([prte_check_tm_pbs_config_run_results=`${prte_check_tm_cv_pbs_config_path} $1 2>&1`],
                     [AS_VAR_COPY([$2], [prte_check_tm_pbs_config_run_results])
                      $3], [$4])
    AS_UNSET([prte_check_tm_pbs_config_run_results])
])


# PRTE_CHECK_TM_LIBS_FLAGS([flags-name-prefix], [input-flags])
# ------------------------------------------------------------
AC_DEFUN([PRTE_CHECK_TM_SPLIT_LIBS_OUTPUT], [
    for prte_check_tm_val in $2; do
        AS_IF([test "`echo $prte_check_tm_val | cut -c1-2`" = "-l"],
              [PRTE_APPEND([$1_LIBS], [${prte_check_tm_val}])],
              [PRTE_APPEND([$1_LDFLAGS], [${prte_check_tm_val}])])
    done
    AS_UNSET([prte_check_tm_val])
])


# PRTE_CHECK_TM_PBS_CONFIG(prefix, [action-if-found], [action-if-not-found],
#                          [action-if-found-but-does-not-work])
# --------------------------------------------------------------------------
AC_DEFUN([PRTE_CHECK_TM_PBS_CONFIG], [
    PRTE_VAR_SCOPE_PUSH([pbs_config_happy])

    pbs_config_happy="yes"

    AC_CACHE_CHECK([for pbs-config path],
                   [prte_check_tm_cv_pbs_config_path],
                   [AS_IF([test -z "${with_tm}" -o "${with_tm}" = "yes"],
                          [prte_check_tm_cv_pbs_config_path="pbs-config"],
                          [prte_check_tm_cv_pbs_config_path="${with_tm}/bin/pbs-config"])])

    AC_CACHE_CHECK([if pbs-config works],
                   [prte_check_tm_cv_pbs_config_works],
                   [PRTE_CHECK_TM_PBS_CONFIG_RUN([--prefix], [prte_check_tm_dummy],
                                                 [prte_check_tm_cv_pbs_config_works="yes"],
                                                 [prte_check_tm_cv_pbs_config_works="no"])])
    AS_IF([test "${prte_check_tm_cv_pbs_config_works}" = "no"], [pbs_config_happy="no"])

    AS_IF([test "${pbs_config_happy}" = "yes"],
          [AC_CACHE_CHECK([for pbs-config cflags],
              [prte_check_tm_cv_pbs_config_cflags_output],
              [PRTE_CHECK_TM_PBS_CONFIG_RUN([--cflags], [prte_check_tm_cv_pbs_config_cflags_output], [],
                   [AC_MSG_ERROR([An error occurred retrieving cflags from pbs-config])])])

           AC_CACHE_CHECK([for pbs-config libs],
              [prte_check_tm_cv_pbs_config_libs_output],
              [PRTE_CHECK_TM_PBS_CONFIG_RUN([--libs], [prte_check_tm_cv_pbs_config_libs_output], [],
                   [AC_MSG_ERROR([An error occurred retrieving libs from pbs-config])])])

           $1_CPPFLAGS="${prte_check_tm_cv_pbs_config_cflags_output}"
           PRTE_CHECK_TM_SPLIT_LIBS_OUTPUT([$1], [${prte_check_tm_cv_pbs_config_libs_output}])

           PRTE_LOG_MSG([pbs-config CPPFLAGS: ${$1_CPPFLAGS}], 1)
           PRTE_LOG_MSG([pbs-config LDFLAGS: ${$1_LDFLAGS}], 1)
           PRTE_LOG_MSG([pbs-config LIBS: ${$1_LIBS}], 1)

           # Now that we supposedly have the right flags, try them out.
           prte_check_tm_CPPFLAGS_save="${CPPFLAGS}"
           prte_check_tm_LDFLAGS_save="${LDFLAGS}"
           prte_check_tm_LIBS_save="${LIBS}"

           CPPFLAGS="${CPPFLAGS} ${$1_CPPFLAGS}"
           LIBS="${LIBS} ${$1_LIBS}"
           LDFLAGS="${LDFLAGS} ${$1_LDFLAGS}"

           pbs_config_happy=no
           AC_CHECK_HEADER([tm.h],
               [AC_CHECK_FUNC([tm_finalize],
                              [pbs_config_happy="yes"])])

           CPPFLAGS="${prte_check_tm_CPPFLAGS_save}"
           LDFLAGS="${prte_check_tm_LDFLAGS_save}"
           LIBS="${prte_check_tm_LIBS_save}"])

    AS_IF([test "${pbs_config_happy}" = "yes"],
          [$1_SUMMARY="yes (${prte_check_tm_cv_pbs_config_path})"
           $2],
          [test "${prte_check_tm_cv_pbs_config_works}" = "yes"],
          [$4],
          [$1_SUMMARY="no"
           $3])

    PRTE_VAR_SCOPE_POP
])


# PRTE_CHECK_TM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PRTE_CHECK_TM],[
    PRTE_VAR_SCOPE_PUSH([prte_check_tm_happy prte_check_tm_found])

    AC_ARG_WITH([tm],
                [AS_HELP_STRING([--with-tm(=DIR)],
                                [Build TM (Torque, PBSPro, and compatible) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([tm-libdir],
                [AS_HELP_STRING([--with-tm-libdir=DIR],
                                [Search for Torque libraries in DIR])])

    AS_IF([test "${with_tm}" = "no"],
          [prte_check_tm_happy="no"],
          [prte_check_tm_happy="yes"])

    prte_check_tm_found=0

    # Note: If we found pbs-config, got flags from it, but those flags don't work, consider that a hard fail
    # for a working TM.  Don't try to search with check package in that case.
    AS_IF([test "${prte_check_tm_happy}" = "yes"],
          [PRTE_CHECK_TM_PBS_CONFIG([$1], [prte_check_tm_found=1], [], [prte_check_tm_happy="no"])])

    # Note that Torque 2.1.0 changed the name of their back-end
    # library to "libtorque".  So we have to check for both libpbs and
    # libtorque.  First, check for libpbs.
    AS_IF([test "${prte_check_tm_happy}" = "yes" -a ${prte_check_tm_found} -eq 0],
          [AS_VAR_SET_IF([prte_cv_check_tm_libs],
              [OAC_CHECK_PACKAGE([tm],
                                 [$1],
                                 [tm.h],
                                 [${prte_cv_check_tm_libs}],
                                 [tm_init],
                                 [prte_check_tm_found=1])],
              [OAC_CHECK_PACKAGE([tm],
                                 [$1],
                                 [tm.h],
                                 [pbs crypto z],
                                 [tm_init],
                                 [prte_cv_check_tm_libs="pbs crypto z"
                                  prte_check_tm_found=1])])
               AS_IF([test ${prte_check_tm_found} -eq 0],
                     [OAC_CHECK_PACKAGE_INVALIDATE_GENERIC_CACHE([tm], [$1], [tm_init])
                      OAC_CHECK_PACKAGE([tm],
                                        [$1],
                                        [tm.h],
                                        [pbs crypto z],
                                        [tm_init],
                                        [prte_cv_check_tm_libs="pbs crypto z"
                                         prte_check_tm_found=1])])
               AS_IF([test ${prte_check_tm_found} -eq 0],
                     [OAC_CHECK_PACKAGE_INVALIDATE_GENERIC_CACHE([tm], [$1], [tm_init])
                      OAC_CHECK_PACKAGE([tm],
                                        [$1],
                                        [tm.h],
                                        [torque],
                                        [tm_init],
                                        [prte_cv_check_tm_libs="torque"
                                         prte_check_tm_found=1])])])

    AS_IF([test ${prte_check_tm_found} -eq 0], [prte_check_tm_happy="no"])


    # Did we find the right stuff?
    AS_IF([test "${prte_check_tm_happy}" = "yes"],
          [$2],
          [AS_IF([test ! -z "${with_tm}" && test "${with_tm}" != "no"],
                 [AC_MSG_ERROR([TM support requested but not found.  Aborting])])
           $3])

    PRTE_SUMMARY_ADD([Resource Managers], [Torque], [], [${$1_SUMMARY}])

    PRTE_VAR_SCOPE_POP
])
