# -*- shell-script -*-
#
# Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved. 
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved. 
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

           # These flags need to get passed to the wrapper compilers
           # (this is unnecessary for the internal/embedded event)

           # Finally, add some flags to the wrapper compiler if we're
           # building with developer headers so that our headers can
           # be found.
           event_external_WRAPPER_EXTRA_CPPFLAGS="$opal_event_external_CPPFLAGS"
           event_external_WRAPPER_EXTRA_LDFLAGS="$opal_event_external_LDFLAGS"
           event_external_WRAPPER_EXTRA_LIBS="$opal_event_external_LIBS"
          ])
])dnl


# MCA_event_external_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_event_external_CONFIG],[
    AC_CONFIG_FILES([opal/mca/event/external/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_event_external_CPPFLAGS_save opal_event_external_CFLAGS_save opal_event_external_LDFLAGS_save opal_event_external_LIBS_save])

    AC_ARG_WITH([external-libevent],
       [AC_HELP_STRING([--with-external-libevent=DIR],
             [Search for event libraries in DIR.  Should only be used if an external copy of libevent is being used.])])

    # Do we want this external component?
    opal_event_external_want=no
    AS_IF([test "$with_external_libevent" != ""], [opal_event_external_want=yes])
 
    # If we still want external support, try it
    AS_IF([test "$opal_event_external_want" = "yes"],
          [OMPI_CHECK_WITHDIR([external-libevent], [$with_event_libdir], 
                              [libevent.*])

           AC_MSG_CHECKING([looking for external event in...])
           AS_IF([test ! -z "$with_external_libevent" -a "$with_external_libevent" != "yes"],
                 [opal_event_libdir="$with_external_libevent"
                  AC_MSG_RESULT([$opal_event_libdir])],
                 [AC_MSG_RESULT([default search paths])])

           opal_event_external_CPPFLAGS_save=$CPPFLAGS
           opal_event_external_CFLAGS_save=$CFLAGS
           opal_event_external_LDFLAGS_save=$LDFLAGS
           opal_event_external_LIBS_save=$LIBS

           OMPI_CHECK_PACKAGE([opal_event_external],
                              [event.h],
                              [event],
                              [event_config_new],
                              [],
                              [$opal_event_libdir],
                              [$opal_event_libdir],
                              [opal_event_external_support=yes],
                              [opal_event_external_support=no])

           CPPFLAGS=$opal_event_external_CPPFLAGS_save
           CFLAGS=$opal_event_external_CFLAGS_save
           LDFLAGS=$opal_event_external_LDFLAGS_save
           LIBS=$opal_event_external_LIBS_save

           # check for pthread support - must do this after
           # restoring LIBS as otherwise the results of
           # this macro will be overwritten
           OMPI_CHECK_FUNC_LIB([evthread_use_pthreads],
                               [event_pthreads],
                               [opal_event_external_has_threads=1],
                               [opal_event_external_has_threads=0])

          ])

    # Done!
    AS_IF([test "$opal_event_external_support" = "yes"],
          [AC_DEFINE_UNQUOTED([EVENT_EXTERNAL_EVENT_VERSION], 
                              [external], 
                              [Version of event])
           AC_DEFINE_UNQUOTED([OPAL_HAVE_EXTERNAL_EVENT_THREAD_SUPPORT],
                              [$opal_event_external_has_threads],
                              [Whether the external libevent has thread support])
           $1],
          [$2])

    AC_SUBST(opal_event_external_CPPFLAGS)
    AC_SUBST(opal_event_external_LDFLAGS)
    AC_SUBST(opal_event_external_LIBS)

    OPAL_VAR_SCOPE_POP
])dnl
