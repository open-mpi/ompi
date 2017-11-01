# -*- shell-script -*-
#
# Copyright (c) 2017 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_notifier_syslog_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_notifier_syslog_CONFIG], [
    AC_CONFIG_FILES([orte/mca/notifier/syslog/Makefile])

    OPAL_VAR_SCOPE_PUSH([orte_notifier_syslog_happy])

    # Per https://github.com/open-mpi/ompi/issues/4373 and
    # https://github.com/open-mpi/ompi/pull/4374, we need to check
    # that syslog.h is compilable.  If syslog.h is not compilable,
    # disable this component.
    AC_CHECK_HEADER([syslog.h],
        [orte_notifier_syslog_happy=1],
        [orte_notifier_syslog_happy=0])

    AS_IF([test $orte_notifier_syslog_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])dnl
