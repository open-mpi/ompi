dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2009      Bull SAS.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([MCA_orte_notifier_CONFIG],[
    ompi_show_subsubtitle "Pre-configuring the framework notifier"

    AC_MSG_CHECKING([if --enable-notifier-log-event was specified])
    AC_ARG_ENABLE(notifier-log-event,
        AC_HELP_STRING([--enable-notifier-log-event],
                       [Enable unusual events notification. (default: disabled)]))
    if test "$enable_notifier_log_event" = "yes"; then
        AC_MSG_RESULT([yes])
        WANT_NOTIFIER_LOG_EVENT=1
    else
        AC_MSG_RESULT([no (disabling "notifier-log-event")])
        WANT_NOTIFIER_LOG_EVENT=0
    fi
    AC_DEFINE_UNQUOTED([ORTE_WANT_NOTIFIER_LOG_EVENT],
                       [$WANT_NOTIFIER_LOG_EVENT],
                       [if the notifier_log_event should be enabled])
    AM_CONDITIONAL([ORTE_WANT_NOTIFIER_LOG_EVENT],
                   [test "$WANT_NOTIFIER_LOG_EVENT" = "1"])

    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)
])
