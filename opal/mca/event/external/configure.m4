# -*- shell-script -*-
#
# Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
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

    OPAL_VAR_SCOPE_PUSH([opal_event_external_CPPFLAGS_save opal_event_external_CFLAGS_save opal_event_external_LDFLAGS_save opal_event_external_LIBS_save])

    AC_ARG_WITH([libevent],
       [AC_HELP_STRING([--with-libevent=DIR],
             [Search for libevent headers and libraries in DIR.  Should only be used if an external copy of libevent is being used.])])

    # Bozo check
    AS_IF([test "$with_libevent" = "no"],
          [AC_MSG_WARN([It is not possible to configure Open MPI --without-libevent])
           AC_MSG_ERROR([Cannot continue])])

    AC_ARG_WITH([libevent-libdir],
       [AC_HELP_STRING([--with-libevent-libdir=DIR],
             [Search for libevent libraries in DIR.  Should only be used if an external copy of libevent is being used.])])

    # Make sure the user didn't specify --with-libevent=internal and
    # --with-libevent-libdir=whatever (because you can only specify
    # --with-libevent-libdir when external libevent is being used).
    AS_IF([test "$with_libevent" = "internal" && test -n "$with_libevent_libdir"],
          [AC_MSG_WARN([Both --with-libevent=internal and --with-libevent-libdir=DIR])
           AC_MSG_WARN([were specified, which does not make sense.])
           AC_MSG_ERROR([Cannot continue])])

    # Do we want this external component? (slightly redundant logic,
    # but hopefully slightly more clear...)
    opal_event_external_want=no
    AS_IF([test "$with_libevent" = "external"], [opal_event_external_want=yes])
    AS_IF([test -n "$with_libevent_libdir"], [opal_event_external_want=yes])
    AS_IF([test -n "$with_libevent" && test "$with_libevent" != "no" && test "$with_libevent" != "internal"], [opal_event_external_want=yes])

    # If we want external support, try it
    AS_IF([test "$opal_event_external_want" = "yes"],
          [ # Error out if the specified dir does not exist
           OPAL_CHECK_WITHDIR([libevent-libdir], [$with_libevent_libdir],
                              [libevent.*])

           AC_MSG_CHECKING([for external libevent in])
           AS_IF([test "$with_libevent" != "external" && test "$with_libevent" != "yes"],
                 [opal_event_dir=$with_libevent
                  AC_MSG_RESULT([$opal_event_dir])
                  OPAL_CHECK_WITHDIR([libevent], [$with_libdir],
                                     [include/event.h])
                 ],
                 [AC_MSG_RESULT([(default search paths)])])
           AS_IF([test ! -z "$with_libevent_libdir" && test "$with_libevent_libdir" != "yes"],
                 [opal_event_libdir="$with_libevent_libdir"])

           opal_event_external_CPPFLAGS_save=$CPPFLAGS
           opal_event_external_CFLAGS_save=$CFLAGS
           opal_event_external_LDFLAGS_save=$LDFLAGS
           opal_event_external_LIBS_save=$LIBS

           OPAL_CHECK_PACKAGE([opal_event_external],
                              [event.h],
                              [event],
                              [event_config_new],
                              [-levent_pthreads],
                              [$opal_event_dir],
                              [$opal_event_libdir],
                              [opal_event_external_support=yes],
                              [opal_event_external_support=no])

           # Ensure that this libevent has the symbol
           # "evthread_set_lock_callbacks", which will only exist if
           # libevent was configured with thread support.
           LIBS="$opal_event_external_LDFLAGS $LIBS"
           AC_CHECK_LIB([event], [evthread_set_lock_callbacks],
                        [],
                        [AC_MSG_WARN([External libevent does not have thread support])
                         AC_MSG_WARN([Open MPI requires libevent to be compiled with])
                         AC_MSG_WARN([thread support enabled])
                         AC_MSG_ERROR([Cannot continue])])
           AC_CHECK_LIB([event_pthreads], [evthread_use_pthreads],
                        [],
                        [AC_MSG_WARN([External libevent does not have thread support])
                         AC_MSG_WARN([Open MPI requires libevent to be compiled with])
                         AC_MSG_WARN([thread support enabled])
                         AC_MSG_ERROR([Cannot continue])])

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
           event_external_WRAPPER_EXTRA_LIBS=$opal_event_external_LIBS
    ])

    # Done!
    AS_IF([test "$opal_event_external_support" = "yes"],
          [ # If we configured successfully, set
           # OPAL_HAVE_WORKING_EVENTOPS to 1 (it's a calculated value
           # in the embedded Open MPI libevent, so we can only assume
           # what it is in the installed libevent :-\ ).
           file=$opal_event_dir/include/libevent/config.h
           OPAL_HAVE_WORKING_EVENTOPS=1
           $1],
          [OPAL_HAVE_WORKING_EVENTOPS=0
           $2])

    OPAL_VAR_SCOPE_POP
])dnl
