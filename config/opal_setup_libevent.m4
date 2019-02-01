# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_libevent_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([OPAL_LIBEVENT_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_libevent_dir opal_libevent_libdir opal_libevent_standard_header_location opal_libevent_standard_lib_location])

    AC_ARG_WITH([libevent],
                [AC_HELP_STRING([--with-libevent=DIR],
                                [Search for libevent headers and libraries in DIR ])])

    AC_ARG_WITH([libevent-libdir],
                [AC_HELP_STRING([--with-libevent-libdir=DIR],
                                [Search for libevent libraries in DIR ])])

    opal_libevent_support=0
    if test "$with_libevent" == "no"; then
        AC_MSG_WARN([PRRTE requires libevent support])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([for libevent in])
    if test ! -z "$with_libevent" && test "$with_libevent" != "yes"; then
        opal_libevent_dir=$with_libevent
        opal_libevent_standard_header_location=no
        opal_libevent_standard_lib_location=no
        AS_IF([test -z "$with_libevent_libdir" || test "$with_libevent_libdir" = "yes"],
              [if test -d $with_libevent/lib; then
                   opal_libevent_libdir=$with_libevent/lib
               elif test -d $with_libevent/lib64; then
                   opal_libevent_libdir=$with_libevent/lib64
               else
                   AC_MSG_RESULT([Could not find $with_libevent/lib or $with_libevent/lib64])
                   AC_MSG_ERROR([Can not continue])
               fi
               AC_MSG_RESULT([$opal_libevent_dir and $opal_libevent_libdir])],
              [AC_MSG_RESULT([$with_libevent_libdir])])
    else
        AC_MSG_RESULT([(default search paths)])
        opal_libevent_standard_header_location=yes
        opal_libevent_standard_lib_location=yes
    fi
    AS_IF([test ! -z "$with_libevent_libdir" && test "$with_libevent_libdir" != "yes"],
          [opal_libevent_libdir="$with_libevent_libdir"
           opal_libevent_standard_lib_location=no])

    OPAL_CHECK_PACKAGE([opal_libevent],
                       [event.h],
                       [event],
                       [event_config_new],
                       [-levent_pthreads],
                       [$opal_libevent_dir],
                       [$opal_libevent_libdir],
                       [opal_libevent_support=1],
                       [opal_libevent_support=0])
    if test $opal_libevent_support = "1"; then
        LIBS="$LIBS $opal_libevent_LIBS"
        OPAL_WRAPPER_FLAGS_ADD([CFLAGS], [$opal_libevent_LIBS])

        if test "$opal_libevent_standard_header_location" != "yes"; then
            CPPFLAGS="$CPPFLAGS $opal_libevent_CPPFLAGS"
            OPAL_WRAPPER_FLAGS_ADD([CPPFLAGS], [$opal_libevent_CPPFLAGS])
        fi
        if test "$opal_libevent_standard_lib_location" != "yes"; then
            LDFLAGS="$LDFLAGS $opal_libevent_LDFLAGS"
            OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$opal_libevent_LDFLAGS])
        fi
    fi

    AC_MSG_CHECKING([will libevent support be built])
    if test "$opal_libevent_support" != "1"; then
        AC_MSG_WARN([LIBEVENT SUPPORT NOT FOUND])
        AC_MSG_ERROR([CANNOT CONTINUE])
    else
        AC_MSG_RESULT([yes])
    fi

    OPAL_SUMMARY_ADD([[Required Packages]],[[libevent]],[libevent],[$opal_libevent_dir])

    OPAL_VAR_SCOPE_POP
])dnl
