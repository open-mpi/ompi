# -*- shell-script -*-
#
# Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
AC_DEFUN([MCA_opal_event_libevent2112_PRIORITY], [80])

dnl
dnl Force this component to compile in static-only mode
dnl
AC_DEFUN([MCA_opal_event_libevent2112_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_opal_event_libevent2112_POST_CONFIG], [
    AM_CONDITIONAL(OPAL_EVENT_HAVE_THREAD_SUPPORT,
                   [test "$enable_event_thread_support" = "yes"])
    AS_IF([test "$1" = "1"],
          [ # Build libevent/include/event2/event-config.h.  If we
           # don't do it here, then libevent's Makefile.am will build
           # it during "make all", which is too late for us (because
           # other things are built before the event framework that
           # end up including event-config.h).  The steps below were
           # effectively copied from libevent's Makefile.am.

           AC_CONFIG_COMMANDS([opal/mca/event/libevent2112/libevent/include/event2/event-config.h],
                              [opal_event_libevent2112_basedir="opal/mca/event/libevent2112"
                               libevent_file="$opal_event_libevent2112_basedir/libevent/include/event2/event-config.h"
                               rm -f "$libevent_file.new"
                               sed -f "$srcdir/$opal_event_libevent2112_basedir/libevent/make-event-config.sed" \
                                   < "$opal_event_libevent2112_basedir/libevent/config.h" >> "$libevent_file.new"

                               # Only make a new .h libevent_file if the
                               # contents haven't changed
                               diff -q $libevent_file "$libevent_file.new" > /dev/null 2> /dev/null
                               if test "$?" = "0"; then
                                   echo $libevent_file is unchanged
                               else
                                   cp "$libevent_file.new" $libevent_file
                               fi
                               rm -f "$libevent_file.new"])

           # Must set this variable so that the framework m4 knows
           # what file to include in opal/mca/event/event.h
           opal_event_base_include="libevent2112/libevent2112.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           libevent_file=$opal_event_libevent2112_basedir/libevent
           CPPFLAGS="-I$OPAL_TOP_SRCDIR/$libevent_file -I$OPAL_TOP_SRCDIR/$libevent_file/include $CPPFLAGS"
           AS_IF([test "$OPAL_TOP_BUILDDIR" != "$OPAL_TOP_SRCDIR"],
                 [CPPFLAGS="-I$OPAL_TOP_BUILDDIR/$libevent_file -I$OPAL_TOP_BUILDDIR/$libevent_file/include $CPPFLAGS"])
           unset libevent_file
          ])
])

dnl MCA_event_libevent2112_CONFIG([action-if-can-compile],
dnl                              [action-if-cant-compile])
dnl ------------------------------------------------
AC_DEFUN([MCA_opal_event_libevent2112_CONFIG],[
    AC_CONFIG_FILES([opal/mca/event/libevent2112/Makefile])
    opal_event_libevent2112_basedir="opal/mca/event/libevent2112"

    # We know that the external event component will be configured
    # before this one because of its priority.  This component is only
    # needed if the external component was not successful in selecting
    # itself.
    AC_MSG_CHECKING([if event external component succeeded])
    AS_IF([test "$opal_event_external_support" = "yes"],
          [AC_MSG_RESULT([yes])
           AC_MSG_NOTICE([event:external succeeded, so this component will be configured, but then will be skipped])
           MCA_opal_event_libevent2112_FAKE_CONFIG($2)],
          [AC_MSG_RESULT([no])
           AC_MSG_NOTICE([event:external failed, so this component will be used])
           MCA_opal_event_libevent2112_REAL_CONFIG($1, $2)])
])

dnl
dnl This macro is invoked when event:external is going to be used (and
dnl this component is *not* going to be used).
dnl
dnl $1: action if this component can compile
dnl (we still invoke $1 so that "make distclean" and friends will work)
dnl
AC_DEFUN([MCA_opal_event_libevent2112_FAKE_CONFIG],[
    MCA_opal_event_libevent2112_SUB_CONFIGURE([], [], [])
    AC_MSG_NOTICE([remember: event:external will be used; this component was configured, but will be skipped])
    $1
])

dnl
dnl This macro has a bunch of side effects.  It is only meant to be
dnl invoked when this component is going to be used (i.e., when
dnl event:external is *not* going to be used).  If this macro is invoked
dnl when event:external is used, Terrible Things will happen.
dnl
dnl $1: action if this component can compile
dnl $2: action if this component cannot compile
dnl
AC_DEFUN([MCA_opal_event_libevent2112_REAL_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([CFLAGS_save CPPFLAGS_save libevent_file event_args libevent_happy])

    CFLAGS_save="$CFLAGS"
    CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"
    CPPFLAGS_save="$CPPFLAGS"
    CPPFLAGS="-I$OPAL_TOP_SRCDIR -I$OPAL_TOP_BUILDDIR -I$OPAL_TOP_SRCDIR/opal/include $CPPFLAGS"

    AC_MSG_CHECKING([libevent configuration args])
    event_args="--disable-dns --disable-http --disable-rpc --disable-openssl --enable-thread-support --disable-evport --disable-gcc-warnings --disable-libevent-regress"

    AC_ARG_ENABLE(event-debug,
                  AC_HELP_STRING([--enable-event-debug], [enable event library debug output]))
    if test "$enable_event_debug" = "yes"; then
        event_args="$event_args --enable-debug-mode"
        CFLAGS="-DUSE_DEBUG $CFLAGS"
    else
        event_args="$event_args --disable-debug-mode"
    fi

    AC_MSG_RESULT([$event_args])

    # Invoke the embedded configure script.
    # We define "random" to be "opal_random" so that Libevent will not
    # use random(3) internally (and potentially unexpectedly perturb
    # values returned by rand(3) to the application).
    CPPFLAGS="$CPPFLAGS -Drandom=opal_random"
    MCA_opal_event_libevent2112_SUB_CONFIGURE([$event_args],
                                              [libevent_happy="yes"],
                                              [libevent_happy="no"])
    if test "$libevent_happy" = "no"; then
        AC_MSG_WARN([Event library failed to configure])
        AC_MSG_ERROR([Cannot continue])
    fi

    # Finally, add some flags to the wrapper compiler if we're
    # building with developer headers so that our headers can
    # be found.
    event_libevent2112_WRAPPER_EXTRA_CPPFLAGS='-I${pkgincludedir}/opal/mca/event/libevent2112/libevent -I${pkgincludedir}/opal/mca/event/libevent2112/libevent/include'

    CFLAGS="$CFLAGS_save"
    CPPFLAGS="$CPPFLAGS_save"

    # With older versions of libevent (e.g., 2.0.22), we had to check
    # if a working eventops was found.  With 2.1.12, we know that it
    # will always be working, so just hard-code the constant to 1.
    OPAL_HAVE_WORKING_EVENTOPS=1

    libevent_file=$opal_event_libevent2112_basedir/libevent/config.h
    AS_IF([test "$libevent_happy" = "yes" && test -r $libevent_file],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])

dnl Call configure in the embedded libevent.
dnl
dnl We still do this so that all the proper GNU Autotools
dnl infrastructure is setup properly (e.g., w.r.t. SUBDIRS=libevent in
dnl this directorys Makefile.am, we still need the Autotools "make
dnl distclean" infrastructure to work properly).
dnl
dnl $1: extra configure arguments
dnl $2: action on success
dnl $3: action on failure
dnl
AC_DEFUN([MCA_opal_event_libevent2112_SUB_CONFIGURE],[
    # We define "random" to be "opal_random" so that Libevent will not
    # use random(3) internally (and potentially unexpectedly perturb
    # values returned by rand(3) to the application).

    OPAL_CONFIG_SUBDIR([$opal_event_libevent2112_basedir/libevent],
        [$1 $opal_subdir_args 'CPPFLAGS=$CPPFLAGS'],
        [$2], [$3])
])
