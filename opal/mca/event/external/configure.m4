# -*- shell-script -*-
#
# Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2015-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Priority
#
AC_DEFUN([MCA_opal_event_external_PRIORITY], [90])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_event_external_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_event_external_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_event_external_POST_CONFIG],[
    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"],
          [AC_DEFINE_UNQUOTED([EVENT_EXTERNAL_EVENT_VERSION],
                              [external],
                              [Version of event])

           # Set this variable so that the framework m4 knows what
           # file to include in opal/mca/event/event.h
           opal_event_external_basedir=opal/mca/event/external
           opal_event_base_include="external/external.h"

           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           CPPFLAGS="$CPPFLAGS $opal_event_external_CPPFLAGS"
           LDFLAGS="$LDFLAGS $opal_event_external_LDFLAGS"
           LIBS="$LIBS $opal_event_external_LIBS"
          ])
])dnl


# MCA_event_external_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_event_external_CONFIG],[
    AC_CONFIG_FILES([opal/mca/event/external/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_event_external_CPPFLAGS_save opal_event_external_CFLAGS_save opal_event_external_LDFLAGS_save opal_event_external_LIBS_save opal_event_dir opal_event_summary_msg])

    opal_event_summary_msg="internal"

    # Check the value of $with_libevent_libdir.  This macro safely
    # handles "yes", "no", blank, and directory name values.
    OPAL_CHECK_WITHDIR([libevent-libdir], [$with_libevent_libdir],
                       [libevent.*])

    # Did the user want us to check for libevent in a specific location?
    AC_MSG_CHECKING([for external libevent in])
    AS_IF([test -n "$with_libevent" && \
           test "$with_libevent" != "external" && \
           test "$with_libevent" != "internal" && \
           test "$with_libevent" != "yes" && \
           test "$with_libevent" != "no"],
          [opal_event_dir=$with_libevent
           AC_MSG_RESULT([$opal_event_dir])
           OPAL_CHECK_WITHDIR([libevent], [$opal_event_dir],
                              [include/event2/event.h])
           AS_IF([test -z "$with_libevent_libdir" || test "$with_libevent_libdir" = "yes"],
                 [AC_MSG_CHECKING([for $with_libevent/lib64])
                  AS_IF([test -d "$with_libevent/lib64"],
                        [opal_event_libdir_found=yes
                         AC_MSG_RESULT([found])],
                        [opal_event_libdir_found=no
                         AC_MSG_RESULT([not found])])
                  AS_IF([test "$opal_event_libdir_found" = "yes"],
                        [opal_event_libdir="$with_libevent/lib64"],
                        [AC_MSG_CHECKING([for $with_libevent/lib])
                         AS_IF([test -d "$with_libevent/lib"],
                               [AC_MSG_RESULT([found])
                                opal_event_libdir="$with_libevent/lib"],
                               [AC_MSG_RESULT([not found])
                                AC_MSG_WARN([Library directories were not found:])
                                AC_MSG_WARN([    $with_libevent/lib64])
                                AC_MSG_WARN([    $with_libevent/lib])
                                AC_MSG_WARN([Please use --with-libevent-libdir to identify it.])
                                AC_MSG_ERROR([Cannot continue])])])])],
          [AC_MSG_RESULT([(default search paths)])])
    AS_IF([test ! -z "$with_libevent_libdir" && test "$with_libevent_libdir" != "yes"],
          [opal_event_libdir="$with_libevent_libdir"])

    AS_IF([test "$with_libevent" != "internal"],
          [opal_event_external_CPPFLAGS_save=$CPPFLAGS
           opal_event_external_CFLAGS_save=$CFLAGS
           opal_event_external_LDFLAGS_save=$LDFLAGS
           opal_event_external_LIBS_save=$LIBS

           OPAL_CHECK_PACKAGE([opal_event_external],
                              [event2/event.h],
                              [event],
                              [event_config_new],
                              [-levent_pthreads],
                              [$opal_event_dir],
                              [$opal_event_libdir],
                              [opal_event_external_support=yes],
                              [opal_event_external_support=no])

           AS_IF([test "$opal_event_external_support" = "yes"],
                 [# Ensure that this libevent has the symbol
                  # "evthread_set_lock_callbacks", which will only exist if
                  # libevent was configured with thread support.
                  LIBS="$opal_event_external_LDFLAGS $LIBS"
                  AC_CHECK_LIB([event], [evthread_set_lock_callbacks],
                               [],
                               [AC_MSG_WARN([External libevent does not have thread support])
                                AC_MSG_WARN([Open MPI requires libevent to be compiled with])
                                AC_MSG_WARN([thread support enabled])
                                opal_event_external_support=no])])

           AS_IF([test "$opal_event_external_support" = "yes"],
                 [AC_CHECK_LIB([event_pthreads], [evthread_use_pthreads],
                               [],
                               [AC_MSG_WARN([External libevent does not have thread support])
                                AC_MSG_WARN([Open MPI requires libevent to be compiled with])
                                AC_MSG_WARN([thread support enabled])
                                opal_event_external_support=no])])

           AS_IF([test "$opal_event_external_support" = "yes"],
                 [AS_IF([test -z "$with_libevent" || test "$with_libevent" = "yes"],
                        [AC_MSG_CHECKING([if external libevent version is 2.0.22 or greater])
                         AC_COMPILE_IFELSE(
                             [AC_LANG_PROGRAM([[#include <event2/event.h>]],
                                 [[
#if defined(_EVENT_NUMERIC_VERSION) && _EVENT_NUMERIC_VERSION < 0x02001600
#error "libevent API version is less than 0x02001600"
#elif defined(EVENT__NUMERIC_VERSION) && EVENT__NUMERIC_VERSION < 0x02001600
#error "libevent API version is less than 0x02001600"
#endif
                                 ]])],
                             [AC_MSG_RESULT([yes])],
                             [AC_MSG_RESULT([no])
                              opal_event_summary_msg="internal (external libevent version is less that internal version 2.0.22)"
                              AC_MSG_WARN([external libevent version is less than internal version (2.0.22)])
                              AC_MSG_WARN([using internal libevent])
                              opal_event_external_support=no])])])

           CPPFLAGS=$opal_event_external_CPPFLAGS_save
           CFLAGS=$opal_event_external_CFLAGS_save
           LDFLAGS=$opal_event_external_LDFLAGS_save
           LIBS=$opal_event_external_LIBS_save

           AC_SUBST(opal_event_external_CPPFLAGS)
           AC_SUBST(opal_event_external_LDFLAGS)
           AC_SUBST(opal_event_external_LIBS)

           # These flags need to get passed to the wrapper compilers
           # (this is unnecessary for the internal/embedded event)
           event_external_WRAPPER_EXTRA_CPPFLAGS=$opal_event_external_CPPFLAGS

           # Finally, add some flags to the wrapper compiler if we're
           # building with developer headers so that our headers can
           # be found.
           event_external_WRAPPER_EXTRA_LDFLAGS=$opal_event_external_LDFLAGS
           event_external_WRAPPER_EXTRA_LIBS=$opal_event_external_LIBS])

##################################################################

    # Done!
    AS_IF([test "$opal_event_external_support" = "yes"],
          [# If we configured successfully, set
           # OPAL_HAVE_WORKING_EVENTOPS to 1 (it's a calculated value
           # in the embedded Open MPI libevent, so we can only assume
           # what it is in the installed libevent :-\ ).
           file=$opal_event_dir/include/libevent/config.h
           OPAL_HAVE_WORKING_EVENTOPS=1
           opal_event_summary_msg="external"
           $1],
          [OPAL_HAVE_WORKING_EVENTOPS=0
           AS_IF([test "$with_libevent" != internal && test -n "$with_libevent"],
                 [AC_MSG_WARN([external libevent requested but cannot be built])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    OPAL_SUMMARY_ADD([[Miscellaneous]],[[Libevent support]], [], [$opal_event_summary_msg])

    OPAL_VAR_SCOPE_POP
])dnl
