# -*- shell-script -*-
#
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.
# Copyright (c) 2021      Nanook Consulting.  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# We have two modes for building libevent.
#
# First is as a co-built libevent.  In this case, Prte's CPPFLAGS
# will be set before configure to include the right -Is to pick up
# libevent headers and LIBS will point to where the .la file for
# libevent will exist.  When co-building, libevent's configure will be
# run already, but the library will not yet be built.  It is ok to run
# any compile-time (not link-time) tests in this mode.  This mode is
# used when the --with-libevent=cobuild option is specified.
#
# Second is an external package.  In this case, all compile and link
# time tests can be run.  This macro must do any CPPFLAGS/LDFLAGS/LIBS
# modifications it desires in order to compile and link against
# libevent.  This mode is used whenever the other modes are not used.
#
# PRTE_LIBEVENT_CONFIG()
# --------------------------------------------------------------------
AC_DEFUN([PRTE_LIBEVENT_CONFIG],[
    PRTE_VAR_SCOPE_PUSH()

    AC_ARG_WITH([libevent],
                [AS_HELP_STRING([--with-libevent=DIR],
                                [Search for libevent headers and libraries in DIR ])])

    AC_ARG_WITH([libevent-libdir],
                [AS_HELP_STRING([--with-libevent-libdir=DIR],
                                [Search for libevent libraries in DIR ])])

    prte_libevent_support=1

    prte_check_libevent_save_CPPFLAGS="$CPPFLAGS"
    prte_check_libevent_save_LDFLAGS="$LDFLAGS"
    prte_check_libevent_save_LIBS="$LIBS"

    # figure out our mode...
    AS_IF([test "$with_libevent" = "cobuild"],
          [_PRTE_LIBEVENT_EMBEDDED_MODE()],
          [_PRTE_LIBEVENT_EXTERNAL([prte_libevent_support=1],
                                   [prte_libevent_support=0])])

    # Check to see if the above check failed because it conflicted with LSF's libevent.so
    # This can happen if LSF's library is in the LDFLAGS envar or default search
    # path. The 'event_getcode4name' function is only defined in LSF's libevent.so and not
    # in Libevent's libevent.so
    #
    # Note that even in a cobuild situation, calling AC_CHECK_LIB is
    # fine here, because we aren't testing Libevent's library, we're
    # effectively testing LSF's library.  We never cobuild LSF
    # (please, may that never happen), so there's no cobuild problem.
    if test $prte_libevent_support -eq 0; then
        AC_CHECK_LIB([event], [event_getcode4name],
                     [AC_MSG_WARN([===================================================================])
                      AC_MSG_WARN([Possible conflicting libevent.so libraries detected on the system.])
                      AC_MSG_WARN([])
                      AC_MSG_WARN([LSF provides a libevent.so that is not from Libevent in its])
                      AC_MSG_WARN([library path. It is possible that you have installed Libevent])
                      AC_MSG_WARN([on the system, but the linker is picking up the wrong version.])
                      AC_MSG_WARN([])
                      AC_MSG_WARN([You will need to address this linker path issue. One way to do so is])
                      AC_MSG_WARN([to make sure the libevent system library path occurs before the])
                      AC_MSG_WARN([LSF library path.])
                      AC_MSG_WARN([===================================================================])
                      ])
    fi

    # need to add resulting flags to global ones so we can
    # test for thread support
    if test ! -z "$prte_libevent_CPPFLAGS"; then
        PRTE_FLAGS_PREPEND_UNIQ(CPPFLAGS, $prte_libevent_CPPFLAGS)
    fi
    if test ! -z "$prte_libevent_LDFLAGS"; then
        PRTE_FLAGS_PREPEND_UNIQ(LDFLAGS, $prte_libevent_LDFLAGS)
    fi
    if test ! -z "$prte_libevent_LIBS"; then
        PRTE_FLAGS_PREPEND_UNIQ(LIBS, $prte_libevent_LIBS)
    fi

    if test $prte_libevent_support -eq 1; then
       AC_MSG_CHECKING([if libevent threads enabled])
       AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
        [
         #include <event.h>
         #include <event2/thread.h>
        ],
        [[
         #if !(EVTHREAD_LOCK_API_VERSION >= 1)
         #  error "No threads!"
         #endif
        ]])],
        [AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])
         AC_MSG_ERROR([PRTE rquires libevent to be compiled with thread support enabled])
	 prte_libevent_support=0])
    fi

    if test $prte_libevent_support -eq 1; then
       AC_MSG_CHECKING([for libevent pthreads support])
       AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
        [
         #include <event.h>
         #include <event2/thread.h>
        ],
        [[
         #if !defined(EVTHREAD_USE_PTHREADS_IMPLEMENTED) || !EVTHREAD_USE_PTHREADS_IMPLEMENTED
         #  error "No pthreads!"
         #endif
        ]])],
        [AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])
         AC_MSG_ERROR([PRTE requires libevent to be compiled with pthread support enabled])
	 prte_libevent_support=0])
    fi

    if test $prte_libevent_support -eq 1; then
        # Pin the "oldest supported" version to 2.0.21
        AC_MSG_CHECKING([if libevent version is 2.0.21 or greater])
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <event2/event.h>]],
                                           [[
                                             #if defined(_EVENT_NUMERIC_VERSION) && _EVENT_NUMERIC_VERSION < 0x02001500
                                             #error "libevent API version is less than 0x02001500"
                                             #elif defined(EVENT__NUMERIC_VERSION) && EVENT__NUMERIC_VERSION < 0x02001500
                                             #error "libevent API version is less than 0x02001500"
                                             #endif
                                           ]])],
                          [AC_MSG_RESULT([yes])],
                          [AC_MSG_RESULT([no])
                           AC_MSG_WARN([libevent version is too old (2.0.21 or later required)])
                           prte_libevent_support=0])
    fi

    PRTE_EVENT_HEADER="<event.h>"
    PRTE_EVENT2_THREAD_HEADER="<event2/thread.h>"

    AC_MSG_CHECKING([will libevent support be built])
    if test $prte_libevent_support -eq 1; then
        AC_MSG_RESULT([yes])
        if test ! -z "$prte_libevent_CPPFLAGS"; then
            PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_CPPFLAGS, $prte_libevent_CPPFLAGS)
        fi
        if test ! -z "$prte_libevent_LDFLAGS"; then
            PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LDFLAGS, $prte_libevent_LDFLAGS)
        fi
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LIBS, $prte_libevent_LIBS)
        # Set output variables
        AC_DEFINE_UNQUOTED([PRTE_EVENT_HEADER], [$PRTE_EVENT_HEADER],
                           [Location of event.h])
        AC_DEFINE_UNQUOTED([PRTE_EVENT2_THREAD_HEADER], [$PRTE_EVENT2_THREAD_HEADER],
                           [Location of event2/thread.h])
        PRTE_SUMMARY_ADD([[Required Packages]],[[Libevent]], [prte_libevent], [yes ($prte_libevent_source)])
    else
        AC_MSG_RESULT([no])
    fi

    # restore global flags
    CPPFLAGS="$prte_check_libevent_save_CPPFLAGS"
    LDFLAGS="$prte_check_libevent_save_LDFLAGS"
    LIBS="$prte_check_libevent_save_LIBS"

    dnl These are needed for backwards comptability with the 2.1 and
    dnl earlier branches.
    AC_DEFINE([PRTE_EVENT_HEADER], [<event.h>], [Location of event.h])
    AC_DEFINE([PRTE_EVENT2_THREAD_HEADER], [<event2/thread.h>], [Location of event.h])

    AC_DEFINE_UNQUOTED([PRTE_HAVE_LIBEVENT], [$prte_libevent_support], [Whether we are building against libevent])

    PRTE_VAR_SCOPE_POP()
])dnl


AC_DEFUN([_PRTE_LIBEVENT_EMBEDDED_MODE], [
    AC_MSG_CHECKING([for libevent])
    AC_MSG_RESULT(["cobuild"])

    AC_ARG_WITH([libevent-cobuild-libs],
                [AS_HELP_STRING([--with-libevent-cobuild-libs=LIBS],
                                [Add the LIBS string to LIBS])])

    prte_libevent_source=cobuild
    PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LIBS], [$with_libevent_cobuild_libs])
])dnl


dnl To support cobuilds, any tests that require linking or running (as
dnl opposed to preprocessing or compiling) should only exist in the
dnl LIBEVENT_EXTERNAL section, since cobuild does not guarantee that a
dnl library will be available at configure time.
AC_DEFUN([_PRTE_LIBEVENT_EXTERNAL],[
    PRTE_VAR_SCOPE_PUSH([libevent_prefix libeventdir_prefix])

    # get rid of any trailing slash(es)
    libevent_prefix=$(echo $with_libevent | sed -e 'sX/*$XXg')
    libeventdir_prefix=$(echo $with_libevent_libdir | sed -e 'sX/*$XXg')

    AS_IF([test ! -z "$libevent_prefix" && test "$libevent_prefix" != "yes"],
          [prte_event_dir="$libevent_prefix"],
          [prte_event_dir=""])

    AS_IF([test ! -z "$libeventdir_prefix" && test "$libeventdir_prefix" != "yes"],
                 [prte_event_libdir="$libeventdir_prefix"],
                 [AS_IF([test ! -z "$libevent_prefix" && test "$libevent_prefix" != "yes"],
                        [if test -d $libevent_prefix/lib64; then
                            prte_event_libdir=$libevent_prefix/lib64
                         elif test -d $libevent_prefix/lib; then
                            prte_event_libdir=$libevent_prefix/lib
                         else
                            AC_MSG_WARN([Could not find $libevent_prefix/lib or $libevent_prefix/lib64])
                            AC_MSG_ERROR([Can not continue])
                         fi
                        ],
                        [prte_event_libdir=""])])

    PRTE_CHECK_PACKAGE([prte_libevent],
                       [event.h],
                       [event_core],
                       [event_config_new],
                       [-levent_pthreads],
                       [$prte_event_dir],
                       [$prte_event_libdir],
                       [$1],
                       [$2],
                       [])

    if test -z "$prte_event_dir"; then
        prte_libevent_source="Standard locations"
    else
        prte_libevent_source=$prte_event_dir
    fi

    PRTE_VAR_SCOPE_POP
])dnl
