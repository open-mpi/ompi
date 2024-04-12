# -*- shell-script -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_prm_hpesmf_CONFIG([action-if-can-compile],
#                            [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_prm_hpesmf_CONFIG], [
    AC_CONFIG_FILES([src/mca/prm/hpesmf/Makefile])

    AS_IF([test "$enable_cray_support" = "yes" &&
           test "$pmix_check_jansson_happy" = "yes" &&
           test "$pmix_check_curl_happy" = "yes"],
          [pmix_check_hpesmf_happy=yes],
          [pmix_check_hpesmf_happy=no])

    PMIX_SUMMARY_ADD([Resource Managers], [HPESMF], [], [$pmix_check_hpesmf_happy (scheduler)])

    AS_IF([test "$pmix_check_hpesmf_happy" = "yes"],
          [$1],
          [$2])
])dnl
