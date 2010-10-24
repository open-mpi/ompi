# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_opal_event_libevent207_PRIORITY], [60])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_event_libevent207_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_event_libevent207_CONFIG(action-if-can-compile, 
#                               [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_event_libevent207_CONFIG],[
    AC_CONFIG_FILES([opal/mca/event/libevent207/Makefile])

    CFLAGS_save="$CFLAGS"
    CFLAGS="$OMPI_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"
    CPPFLAGS_save="$CPPFLAGS"
    CPPFLAGS="-I$OMPI_TOP_SRCDIR -I$OMPI_TOP_BUILDDIR -I$OMPI_TOP_SRCDIR/opal/include $CPPFLAGS"
    
    AC_MSG_CHECKING([libevent configuration args])

    str=`event_args="--disable-dns --disable-http --disable-rpc --disable-openssl --enable-hidden-symbols --includedir=$includedir/openmpi/opal/event/libevent/include"`
    eval $str
    unset str

    AC_ARG_ENABLE(event-rtsig,
        AC_HELP_STRING([--enable-event-rtsig],
                       [enable support for real time signals (experimental)]))
    if test "$enable_event_rtsig" = "yes"; then
        event_args="$event_args --enable-rtsig"
    fi

    AC_ARG_ENABLE(event-select,
                  AC_HELP_STRING([--disable-event-select], [disable select support]))
    if test "$enable_event_select" = "no"; then
        event_args="$event_args --disable-select"
    fi

    AC_ARG_ENABLE(event-poll,
                  AC_HELP_STRING([--disable-event-poll], [disable poll support]))
    if test "$enable_event_poll" = "no"; then
        event_args="$event_args --disable-poll"
    fi

    AC_ARG_ENABLE(event-devpoll,
                  AC_HELP_STRING([--disable-event-devpoll], [disable devpoll support]))
    if test "$enable_event_devpoll" = "no"; then
        event_args="$event_args --disable-devpoll"
    fi

    AC_ARG_ENABLE(event-kqueue,
                  AC_HELP_STRING([--disable-event-kqueue], [disable kqueue support]))
    if test "$enable_event_kqueue" = "no"; then
        event_args="$event_args --disable-kqueue"
    fi

    AC_ARG_ENABLE(event-epoll,
                  AC_HELP_STRING([--disable-event-epoll], [disable epoll support]))
    if test "$enable_event_epoll" = "no"; then
        event_args="$event_args --disable-epoll"
    fi

    AC_ARG_ENABLE(event-evport,
                  AC_HELP_STRING([--enable-event-evport], [enable evport support]))
    if test "$enable_event_evport" = "yes"; then
        event_args="$event_args --enable-evport"
    else
        event_args="$event_args --disable-evport"
    fi

    AC_ARG_ENABLE(event-signal,
                  AC_HELP_STRING([--disable-event-signal], [disable signal support]))
    if test "$enable_event_signal" = "no"; then
        event_args="$event_args --disable-signal"
    fi

    AC_ARG_ENABLE(event-debug,
                  AC_HELP_STRING([--enable-event-debug], [enable event library debug output]))
    if test "$enable_event_debug" = "no"; then
        event_args="$event_args --disable-debug-mode"
    fi

    AC_MSG_RESULT([$event_args])

    OMPI_CONFIG_SUBDIR(opal/mca/event/libevent207/libevent, 
        [$event_args $ompi_subdir_args],
        [libevent_happy="yes"], [libevent_happy="no"])
    if test "$libevent_happy" = "no"; then
        AC_MSG_WARN([Event library failed to configure])
        AC_MSG_ERROR([Cannot continue])
    fi

    CFLAGS="$CFLAGS_save"
    CPPFLAGS="$CPPFLAGS_save"

    # If we configured successfully, set OPAL_HAVE_WORKING_EVENTOPS to
    # the value in the generated libevent/config.h (NOT
    # libevent/include/event2/event-config.h!  That file is generated
    # during "make" -- libevent.h is an AC_HEADER_FILE).  Otherwise,
    # set it to 0.
    file=opal/mca/event/libevent207/libevent/config.h
    AS_IF([test "$libevent_happy" = "yes" -a -r $file], 
          [$1
           OPAL_HAVE_WORKING_EVENTOPS=`grep HAVE_WORKING_EVENTOPS $file | awk '{print [$]3 }'`], 
          [$2
           OPAL_HAVE_WORKING_EVENTOPS=0])
    unset file
])
