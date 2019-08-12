# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2017-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
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

    pmix_libevent_support=0

    AS_IF([test "$pmix_mode" = "embedded"],
          [_PMIX_LIBEVENT_EMBEDDED_MODE],
          [AS_IF([test $pmix_libev_support -eq 0],
                 [_PMIX_LIBEVENT_EXTERNAL])])

    if test $pmix_libevent_support -eq 1; then
        AC_MSG_CHECKING([libevent header])
        AC_DEFINE_UNQUOTED([PMIX_EVENT_HEADER], [$PMIX_EVENT_HEADER],
                           [Location of event.h])
        AC_MSG_RESULT([$PMIX_EVENT_HEADER])
        AC_MSG_CHECKING([libevent2/thread header])
        AC_DEFINE_UNQUOTED([PMIX_EVENT2_THREAD_HEADER], [$PMIX_EVENT2_THREAD_HEADER],
                           [Location of event2/thread.h])
        AC_MSG_RESULT([$PMIX_EVENT2_THREAD_HEADER])

        PMIX_SUMMARY_ADD([[External Packages]],[[Libevent]], [pmix_libevent], [yes ($pmix_libevent_source)])
    fi
])

AC_DEFUN([_PMIX_LIBEVENT_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for libevent])
    AC_MSG_RESULT([assumed available (embedded mode)])

    AS_IF([test -z "$with_libevent_header" || test "$with_libevent_header" = "yes"],
          [PMIX_EVENT_HEADER="<event.h>"
           PMIX_EVENT2_THREAD_HEADER="<event2/thread.h>"],
          [PMIX_EVENT_HEADER="$with_libevent_header"
           PMIX_EVENT2_THREAD_HEADER="$with_libevent_header"])

    pmix_libevent_source=embedded
    pmix_libevent_support=1
])

AC_DEFUN([_PMIX_LIBEVENT_EXTERNAL],[
    PMIX_VAR_SCOPE_PUSH([pmix_event_dir pmix_event_libdir pmix_event_defaults])

    AC_ARG_WITH([libevent],
                [AC_HELP_STRING([--with-libevent=DIR],
                                [Search for libevent headers and libraries in DIR ])])

    AC_ARG_WITH([libevent-libdir],
                [AC_HELP_STRING([--with-libevent-libdir=DIR],
                                [Search for libevent libraries in DIR ])])

    pmix_check_libevent_save_CPPFLAGS="$CPPFLAGS"
    pmix_check_libevent_save_LDFLAGS="$LDFLAGS"
    pmix_check_libevent_save_LIBS="$LIBS"

    # get rid of the trailing slash(es)
    libevent_prefix=$(echo $with_libevent | sed -e 'sX/*$XXg')
    libeventdir_prefix=$(echo $with_libevent_libdir | sed -e 'sX/*$XXg')

    if test "$libevent_prefix" != "no"; then
        AC_MSG_CHECKING([for libevent in])
        if test ! -z "$libevent_prefix" && test "$libevent_prefix" != "yes"; then
            pmix_event_defaults=no
            pmix_event_dir=$libevent_prefix/include
            if test -d $libevent_prefix/lib; then
                pmix_event_libdir=$libevent_prefix/lib
            elif test -d $libevent_prefix/lib64; then
                pmix_event_libdir=$libevent_prefix/lib64
            elif test -d $libevent_prefix; then
                pmix_event_libdir=$libevent_prefix
            else
                AC_MSG_RESULT([Could not find $libevent_prefix/lib, $libevent_prefix/lib64, or $libevent_prefix])
                AC_MSG_ERROR([Can not continue])
            fi
            AC_MSG_RESULT([$pmix_event_dir and $pmix_event_libdir])
        else
            pmix_event_defaults=yes
            pmix_event_dir=/usr/include
            if test -d /usr/lib; then
                pmix_event_libdir=/usr/lib
                AC_MSG_RESULT([(default search paths)])
            elif test -d /usr/lib64; then
                pmix_event_libdir=/usr/lib64
                AC_MSG_RESULT([(default search paths)])
            else
                AC_MSG_RESULT([default paths not found])
                pmix_libevent_support=0
            fi
        fi
        AS_IF([test ! -z "$libeventdir_prefix" && "$libeventdir_prefix" != "yes"],
              [pmix_event_libdir="$libeventdir_prefix"])

        PMIX_CHECK_PACKAGE([pmix_libevent],
                           [event.h],
                           [event],
                           [event_config_new],
                           [-levent -levent_pthreads],
                           [$pmix_event_dir],
                           [$pmix_event_libdir],
                           [pmix_libevent_support=1],
                           [pmix_libevent_support=0])

        AS_IF([test "$pmix_event_defaults" = "no"],
              [PMIX_FLAGS_APPEND_UNIQ(CPPFLAGS, $pmix_libevent_CPPFLAGS)
               PMIX_FLAGS_APPEND_UNIQ(LDFLAGS, $pmix_libevent_LDFLAGS)])
        PMIX_FLAGS_APPEND_UNIQ(LIBS, $pmix_libevent_LIBS)

        if test $pmix_libevent_support -eq 1; then
            # Ensure that this libevent has the symbol
            # "evthread_set_lock_callbacks", which will only exist if
            # libevent was configured with thread support.
            AC_CHECK_LIB([event], [evthread_set_lock_callbacks],
                         [],
                         [AC_MSG_WARN([External libevent does not have thread support])
                          AC_MSG_WARN([PMIx requires libevent to be compiled with])
                          AC_MSG_WARN([thread support enabled])
                          pmix_libevent_support=0])
        fi
        if test $pmix_libevent_support -eq 1; then
            AC_CHECK_LIB([event_pthreads], [evthread_use_pthreads],
                         [],
                         [AC_MSG_WARN([External libevent does not have thread support])
                          AC_MSG_WARN([PMIx requires libevent to be compiled with])
                          AC_MSG_WARN([thread support enabled])
                          pmix_libevent_support=0])
        fi
    fi

    CPPFLAGS="$pmix_check_libevent_save_CPPFLAGS"
    LDFLAGS="$pmix_check_libevent_save_LDFLAGS"
    LIBS="$pmix_check_libevent_save_LIBS"

    AC_MSG_CHECKING([will libevent support be built])
    if test $pmix_libevent_support -eq 1; then
        AC_MSG_RESULT([yes])
        # Set output variables
        PMIX_EVENT_HEADER="<event.h>"
        PMIX_EVENT2_THREAD_HEADER="<event2/thread.h>"
        AC_DEFINE_UNQUOTED([PMIX_EVENT_HEADER], [$PMIX_EVENT_HEADER],
                           [Location of event.h])
        pmix_libevent_source=$pmix_event_dir
        AS_IF([test "$pmix_event_defaults" = "no"],
              [PMIX_FLAGS_APPEND_UNIQ(CPPFLAGS, $pmix_libevent_CPPFLAGS)
               PMIX_FLAGS_APPEND_UNIQ(LDFLAGS, $pmix_libevent_LDFLAGS)])
        PMIX_FLAGS_APPEND_UNIQ(LIBS, $pmix_libevent_LIBS)
    else
        AC_MSG_RESULT([no])
    fi

    AC_DEFINE_UNQUOTED([PMIX_HAVE_LIBEVENT], [$pmix_libevent_support], [Whether we are building against libevent])

    PMIX_VAR_SCOPE_POP
])dnl
