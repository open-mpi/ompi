# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
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

    AS_IF([test "$enable_embedded_mode" = "yes"],
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

    CPPFLAGS="$CPPFLAGS $PMIX_EVENT_CPPFLAGS"
    LDFLAGS="$LDFLAGS $PMIX_EVENT_LDFLAGS"
    LIBS="$LIBS $PMIX_EVENT_LIBS"
])

AC_DEFUN([_PMIX_LIBEVENT_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for libevent])
    AC_MSG_RESULT([assumed available (embedded mode)])

    PMIX_EVENT_HEADER="$with_libevent_header"
    PMIX_EVENT2_THREAD_HEADER="$with_libevent_header"
    PMIX_EVENT_CPPFLAGS=
    PMIX_EVENT_LIB=
    PMIX_EVENT_LDFLAGS=

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

    CPPFLAGS="$pmix_libevent_CPPFLAGS $CPPFLAGS"
    LIBS="$pmix_libevent_LIBS $LIBS"
    LDFLAGS="$pmix_libevent_LDFLAGS $LDFLAGS"


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
    # Chck if this libevent has the symbol
    # "libevent_global_shutdown", which will only exist in
    # libevent version 2.1.1+
    AC_CHECK_FUNCS([libevent_global_shutdown],[], [])

    # Set output variables
    PMIX_EVENT_HEADER="<event.h>"
    PMIX_EVENT2_THREAD_HEADER="<event2/thread.h>"
    PMIX_EVENT_LIB=-levent
    AS_IF([test "$pmix_event_dir" != ""],
        [PMIX_EVENT_CPPFLAGS="-I$pmix_event_dir/include"])
    AS_IF([test "$pmix_event_libdir" != ""],
        [PMIX_EVENT_LDFLAGS="-L$pmix_event_libdir"])

    PMIX_VAR_SCOPE_POP
])dnl
