dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  Components must set
# their priorities in their configure.m4 files.  They must also set
# the shell variable $opal_event_<component>_include to a header file
# name (relative to opal/mca/event) that will be included in
# opal/mca/event/event.h.

# Optionally, components may also set the following shell variables:
#
# opal_event_<component>_ADD_CPPFLAGS
# opal_event_<component>_ADD_LDFLAGS
# opal_event_<component>_ADD_LIBS
# opal_event_<component>_ADD_WRAPPER_EXTRA_CPPFLAGS
# opal_event_<component>_ADD_WRAPPER_EXTRA_LDFLAGS
# opal_event_<component>_ADD_WRAPPER_EXTRA_LIBS
#
# The first 3 will be added to the over all CPPFLAGS/LDFLAGS/LIBS if
# that component is chosen as the winning component.  Similarly, the
# latter 3 will be added to WRAPPER_EXTRA_* if that component wins.

dnl We only want one winning component.
m4_define(MCA_opal_event_CONFIGURE_MODE, STOP_AT_FIRST_PRIORITY)

AC_DEFUN([MCA_opal_event_CONFIG],[
    # configure all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    # We must have found exactly 1 static component, or we can't
    # continue.  STOP_AT_FIRST_PRIORITY will guarantee that we find at
    # most one.  We need to check here that we found *at least* one.
    AS_IF([test "$MCA_opal_event_STATIC_COMPONENTS" = ""],
          [AC_MSG_WARN([Did not find a suitable static opal event component])
           AC_MSG_ERROR([Cannot continue])])

    # If there's any spaces in the middle of the string, then we found
    # more than 1 eligible static component.  That's no good (and
    # should never happen, but let's be sure)!
    opal_event_winner=`echo $MCA_opal_event_STATIC_COMPONENTS | sed -e 's/^[ ]+//' | sed -e 's/[ ]+$//'`

    AS_IF([test "`echo $opal_event_winner | sed 's/ //'`" != "$opal_event_winner"],
          [AC_MSG_WARN([Found more than 1 eligible static opal event component])
           AC_MSG_WARN([This should never happen!])
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
    eval "opal_event_base_include=\`echo \$opal_event_${opal_event_winner}_include\`"
    AS_IF([test "$opal_event_base_include" = ""],
          [AC_MSG_RESULT([missing])
           AC_MSG_WARN([Missing implementation header])
           AC_MSG_ERROR([Cannot continue])])
    AC_MSG_RESULT([$opal_event_base_include])

    AC_DEFINE_UNQUOTED([MCA_event_IMPLEMENTATION_HEADER],
                       ["opal/mca/event/$opal_event_base_include"],
                       [Header to include for event implementation])

    # See if they set any flags for us
    _MCA_opal_event_base_flags([CPPFLAGS], [CPPFLAGS])
    _MCA_opal_event_base_flags([LDFLAGS], [LDFLAGS])
    _MCA_opal_event_base_flags([LIBS], [LIBS])
    _MCA_opal_event_base_flags([wrapper CPPFLAGS], [WRAPPER_EXTRA_CPPFLAGS])
    _MCA_opal_event_base_flags([wrapper LDFLAGS], [WRAPPER_EXTRA_LDFLAGS])
    _MCA_opal_event_base_flags([wrapper LIBS], [WRAPPER_EXTRA_LIBS])
])

dnl Helper function
dnl $1 = message to display
dnl $2 = output variable to set / input variable suffix
AC_DEFUN([_MCA_opal_event_base_flags],[
        AC_MSG_CHECKING([for winning event component additional $1])
        eval "opal_event_base_tmp=\`echo \$opal_event_${opal_event_winner}_ADD_$2\`"
        AS_IF([test "$opal_event_base_tmp" != ""],
              [AC_MSG_RESULT([$opal_event_base_tmp])
               $2="[$]$2 $opal_event_base_tmp"],
              [AC_MSG_RESULT([none])])
])dnl
