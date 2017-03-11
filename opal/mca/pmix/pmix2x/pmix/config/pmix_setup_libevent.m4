# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_libevent_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([PMIX_LIBEVENT_CONFIG],[
    AC_ARG_WITH([libevent-header],
                [AC_HELP_STRING([--with-libevent-header=HEADER],
                                [The value that should be included in C files to include event.h])])

    AC_ARG_ENABLE([embedded-libevent],
                  [AC_HELP_STRING([--enable-embedded-libevent],
                                  [Enable use of locally embedded libevent])])

    AS_IF([test "$enable_embedded_libevent" = "yes"],
          [_PMIX_LIBEVENT_EMBEDDED_MODE],
          [_PMIX_LIBEVENT_EXTERNAL])

    AC_MSG_CHECKING([libevent header])
    AC_DEFINE_UNQUOTED([PMIX_EVENT_HEADER], [$PMIX_EVENT_HEADER],
                       [Location of event.h])
    AC_MSG_RESULT([$PMIX_EVENT_HEADER])
    AC_MSG_CHECKING([libevent2/thread header])
    AC_DEFINE_UNQUOTED([PMIX_EVENT2_THREAD_HEADER], [$PMIX_EVENT2_THREAD_HEADER],
                       [Location of event2/thread.h])
    AC_MSG_RESULT([$PMIX_EVENT2_THREAD_HEADER])
])

AC_DEFUN([_PMIX_LIBEVENT_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for libevent])
    AC_MSG_RESULT([assumed available (embedded mode)])

    PMIX_EVENT_HEADER="$with_libevent_header"
    PMIX_EVENT2_THREAD_HEADER="$with_libevent_header"

 ])

AC_DEFUN([_PMIX_LIBEVENT_EXTERNAL],[
    PMIX_VAR_SCOPE_PUSH([pmix_event_dir pmix_event_libdir])

    AC_ARG_WITH([libevent],
                [AC_HELP_STRING([--with-libevent=DIR],
                                [Search for libevent headers and libraries in DIR ])])

    # Bozo check
    AS_IF([test "$with_libevent" = "no"],
          [AC_MSG_WARN([It is not possible to configure PMIx --without-libevent])
           AC_MSG_ERROR([Cannot continue])])

    AC_ARG_WITH([libevent-libdir],
                [AC_HELP_STRING([--with-libevent-libdir=DIR],
                                [Search for libevent libraries in DIR ])])

    AC_MSG_CHECKING([for libevent in])
    if test ! -z "$with_libevent" && test "$with_libevent" != "yes"; then
        pmix_event_dir=$with_libevent
        if test -d $with_libevent/lib; then
            pmix_event_libdir=$with_libevent/lib
        elif test -d $with_libevent/lib64; then
            pmix_event_libdir=$with_libevent/lib64
        else
            AC_MSG_RESULT([Could not find $with_libevent/lib or $with_libevent/lib64])
            AC_MSG_ERROR([Can not continue])
        fi
        AC_MSG_RESULT([$pmix_event_dir and $pmix_event_libdir])
    else
        AC_MSG_RESULT([(default search paths)])
    fi
    AS_IF([test ! -z "$with_libevent_libdir" && "$with_libevent_libdir" != "yes"],
          [pmix_event_libdir="$with_libevent_libdir"])

    PMIX_CHECK_PACKAGE([pmix_libevent],
                       [event.h],
                       [event],
                       [event_config_new],
                       [-levent -levent_pthreads],
                       [$pmix_event_dir],
                       [$pmix_event_libdir],
                       [],
                       [AC_MSG_WARN([LIBEVENT SUPPORT NOT FOUND])
                        AC_MSG_ERROR([CANNOT CONTINE])])

    PMIX_FLAGS_APPEND_UNIQ(CPPFLAGS, $pmix_libevent_CPPFLAGS)
    PMIX_FLAGS_APPEND_UNIQ(LIBS, $pmix_libevent_LIBS)
    PMIX_FLAGS_APPEND_UNIQ(LDFLAGS, $pmix_libevent_LDFLAGS)

    # Ensure that this libevent has the symbol
    # "evthread_set_lock_callbacks", which will only exist if
    # libevent was configured with thread support.
    AC_CHECK_LIB([event], [evthread_set_lock_callbacks],
                 [],
                 [AC_MSG_WARN([External libevent does not have thread support])
                  AC_MSG_WARN([PMIx requires libevent to be compiled with])
                  AC_MSG_WARN([thread support enabled])
                  AC_MSG_ERROR([Cannot continue])])
    AC_CHECK_LIB([event_pthreads], [evthread_use_pthreads],
                 [],
                 [AC_MSG_WARN([External libevent does not have thread support])
                  AC_MSG_WARN([PMIx requires libevent to be compiled with])
                  AC_MSG_WARN([thread support enabled])
                  AC_MSG_ERROR([Cannot continue])])

    # Set output variables
    PMIX_EVENT_HEADER="<event.h>"
    PMIX_EVENT2_THREAD_HEADER="<event2/thread.h>"

    PMIX_VAR_SCOPE_POP
])dnl
