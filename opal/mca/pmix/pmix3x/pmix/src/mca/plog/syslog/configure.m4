# -*- shell-script -*-
#
# Copyright (c) 2017      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_plog_syslog_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_pmix_plog_syslog_CONFIG], [
    AC_CONFIG_FILES([src/mca/plog/syslog/Makefile])

    PMIX_VAR_SCOPE_PUSH([pmix_plog_syslog_happy])

    # if syslog.h is not compilable,
    # disable this component.
    AC_CHECK_HEADER([syslog.h],
        [pmix_plog_syslog_happy=1],
        [pmix_plog_syslog_happy=0])

    AS_IF([test $pmix_plog_syslog_happy -eq 1],
          [$1],
          [$2])

    PMIX_VAR_SCOPE_POP
])dnl
