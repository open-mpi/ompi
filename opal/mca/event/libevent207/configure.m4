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

# MCA_event_libevent207_CONFIG([action-if-can-compile], 
#                              [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_event_libevent207_CONFIG],[
    AC_CONFIG_FILES([opal/mca/event/libevent207/Makefile])
    basedir="opal/mca/event/libevent207"

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

    AC_ARG_ENABLE(event-thread-support,
                  AC_HELP_STRING([--enable-event-thread-support], [enable event library internal thread support]))
    if test "$enable_event_thread_support" = "no"; then
        event_args="$event_args --disable-thread-support"
    fi

    AC_MSG_RESULT([$event_args])

    OMPI_CONFIG_SUBDIR([$basedir/libevent], 
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
    # libevent/include/event2/event-config.h!).  Otherwise, set it to
    # 0.
    file=$basedir/libevent/config.h
    AS_IF([test "$libevent_happy" = "yes" -a -r $file], 
          [OPAL_HAVE_WORKING_EVENTOPS=`grep HAVE_WORKING_EVENTOPS $file | awk '{print [$]3 }'`

           # Build libevent/include/event2/event-config.h.  If we
           # don't do it here, then libevent's Makefile.am will build
           # it during "make all", which is too late for us (because
           # other things are built before the event framework that
           # end up including event-config.h).  The steps below were
           # copied from libevent's Makefile.am.
           file="$basedir/libevent/include/event2/event-config.h"
           mkdir -p "`dirname $OMPI_TOP_SRCDIR/$file`"
           AS_IF([test ! -d "`dirname $OMPI_TOP_SRCDIR/$file`"],
                 [AC_MSG_WARN([Failed to mkdir `dirname $file`])
                  AC_MSG_ERROR([Cannot continue])])

           cat > "$file.in" <<EOF
/* event2/event-config.h
 *
 * This file was generated by autoconf when libevent was built, and
 * post- processed by Open MPI's component configure.m4 (so that
 * Libevent wouldn't build it during "make all") so that its macros
 * would have a uniform prefix.
 *
 * DO NOT EDIT THIS FILE.
 *
 * Do not rely on macros in this file existing in later versions
 */
#ifndef _EVENT2_EVENT_CONFIG_H_
#define _EVENT2_EVENT_CONFIG_H_
EOF

           sed -e 's/#define /#define _EVENT_/' \
               -e 's/#undef /#undef _EVENT_/' \
               -e 's/#ifndef /#ifndef _EVENT_/' < "$basedir/libevent/config.h" >> "$file.in"
           echo "#endif" >> "$file.in"
           AC_CONFIG_FILES([$file])

           # Must set this variable so that the framework m4 knows
           # what file to include in opal/mca/event/event.h
           event_base_include="libevent207/libevent207.h"

           # We also set _cppflags so that when including
           # libevent207.h, it can find all the actual libevent files.
           # Be a little friendly and only include the -I for the
           # builddir if it's different than the srcdir.
           file=$basedir/libevent
           event_base_include_cppflags="-I$OMPI_TOP_SRCDIR/$file -I$OMPI_TOP_SRCDIR/$file/include"
           AS_IF([test "$OMPI_TOP_BUILDDIR" != "$OMPI_TOP_SRCDIR"],
                 [event_base_include_cppflags="$event_base_include_cpp_flags -I$OMPI_TOP_BUILDDIR/$file/include"])
           if test "$with_devel_headers" = "yes" ; then
               WRAPPER_EXTRA_CPPFLAGS="$WRAPPER_EXTRA_CPPFLAGS "'-I${includedir}/openmpi/opal/mca/event/libevent207/libevent -I${includedir}/openmpi/opal/mca/event/libevent207/libevent/include'
           fi
           $1],
          [$2
           OPAL_HAVE_WORKING_EVENTOPS=0])
    unset file
])
