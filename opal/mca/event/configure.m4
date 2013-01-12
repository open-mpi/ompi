dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  Components must set
# their priorities in their configure.m4 files.  They must also set
# the shell variable $opal_event_base_include to a header file name
# (relative to opal/mca/event) that will be included in
# opal/mca/event/event.h.

dnl We only want one winning component (vs. STOP_AT_FIRST_PRIORITY,
dnl which will allow all components of the same priority who succeed to
dnl win)
m4_define(MCA_opal_event_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_opal_event_CONFIG],[
    opal_event_base_include=
    
    # configure all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    # We must have found exactly 1 static component, or we can't
    # continue.  STOP_AT_FIRST will guarantee that we find at most
    # one.  We need to check here that we found *at least* one.
    AS_IF([test "$MCA_opal_event_STATIC_COMPONENTS" = ""],
          [AC_MSG_WARN([Did not find a suitable static opal event component])
           AC_MSG_ERROR([Cannot continue])])

    # We need to find out what the underlying component set
    # HAVE_WORKING_EVENTOPS was set to.  This is for systems that
    # don't have working poll(), etc. (e.g., Cray) -- we still need an
    # event component (for timers, etc.), but we don't have working
    # event ops.  Ensure that it was set by the component.
    echo " "
    AC_MSG_CHECKING([if have working event ops for the event framework])
    AS_IF([test "$OPAL_HAVE_WORKING_EVENTOPS" = ""],
          [AC_MSG_RESULT([unknown])
           AC_MSG_WARN([Event component did not set OPAL_HAVE_WORKING_EVENTOPS])
           AC_MSG_ERROR([Cannot continue])],
          [AS_IF([test "$OPAL_HAVE_WORKING_EVENTOPS" = "1"],
                 [AC_MSG_RESULT([yes])],
                 [AC_MSG_RESULT([no])])])
    AC_DEFINE_UNQUOTED(OPAL_HAVE_WORKING_EVENTOPS, 
                       [$OPAL_HAVE_WORKING_EVENTOPS], 
                       [Whether our event component has working event operations or not (if not, then assumedly it only has working timers and signals)])

    # The winning component will have told us where their header file
    # is located
    AC_MSG_CHECKING([for winning event component header file])
    AS_IF([test "$opal_event_base_include" = ""],
          [AC_MSG_RESULT([missing])
           AC_MSG_WARN([Missing implementation header])
           AC_MSG_ERROR([Cannot continue])])
    AC_DEFINE_UNQUOTED([MCA_event_IMPLEMENTATION_HEADER],
                       ["opal/mca/event/$opal_event_base_include"],
                       [Header to include for event implementation])
    AC_MSG_RESULT([$opal_event_base_include])
])dnl
