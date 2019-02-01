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

# MCA_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([OPAL_HWLOC_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_hwloc_dir opal_hwloc_libdir opal_hwloc_standard_header_location opal_hwloc_standard_lib_location])

    AC_ARG_WITH([hwloc],
                [AC_HELP_STRING([--with-hwloc=DIR],
                                [Search for hwloc headers and libraries in DIR ])])

    AC_ARG_WITH([hwloc-libdir],
                [AC_HELP_STRING([--with-hwloc-libdir=DIR],
                                [Search for hwloc libraries in DIR ])])

    opal_hwloc_support=0
    if test "$with_hwloc" == "no"; then
        AC_MSG_WARN([PRRTE requires hwloc support])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([for hwloc in])
    if test ! -z "$with_hwloc" && test "$with_hwloc" != "yes"; then
        opal_hwloc_dir=$with_hwloc
        opal_hwloc_standard_header_location=no
        opal_hwloc_standard_lib_location=no
        AS_IF([test -z "$with_hwloc_libdir" || test "$with_hwloc_libdir" = "yes"],
              [if test -d $with_hwloc/lib; then
                   opal_hwloc_libdir=$with_hwloc/lib
               elif test -d $with_hwloc/lib64; then
                   opal_hwloc_libdir=$with_hwloc/lib64
               else
                   AC_MSG_RESULT([Could not find $with_hwloc/lib or $with_hwloc/lib64])
                   AC_MSG_ERROR([Can not continue])
               fi
               AC_MSG_RESULT([$opal_hwloc_dir and $opal_hwloc_libdir])],
              [AC_MSG_RESULT([$with_hwloc_libdir])])
    else
        AC_MSG_RESULT([(default search paths)])
        opal_hwloc_standard_header_location=yes
        opal_hwloc_standard_lib_location=yes
    fi
    AS_IF([test ! -z "$with_hwloc_libdir" && test "$with_hwloc_libdir" != "yes"],
          [opal_hwloc_libdir="$with_hwloc_libdir"
           opal_hwloc_standard_lib_location=no])

    OPAL_CHECK_PACKAGE([opal_hwloc],
                       [hwloc.h],
                       [hwloc],
                       [hwloc_topology_init],
                       [],
                       [$opal_hwloc_dir],
                       [$opal_hwloc_libdir],
                       [opal_hwloc_support=1],
                       [opal_hwloc_support=0])
    if test $opal_hwloc_support = "1"; then
        LIBS="$LIBS $opal_hwloc_LIBS"
        OPAL_WRAPPER_FLAGS_ADD([CFLAGS], [$opal_hwloc_LIBS])

        if test "$opal_hwloc_standard_header_location" != "yes"; then
            CPPFLAGS="$CPPFLAGS $opal_hwloc_CPPFLAGS"
            OPAL_WRAPPER_FLAGS_ADD([CPPFLAGS], [$opal_hwloc_CPPFLAGS])
        fi
        if test "$opal_hwloc_standard_lib_location" != "yes"; then
            LDFLAGS="$LDFLAGS $opal_hwloc_LDFLAGS"
            OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$opal_hwloc_LDFLAGS])
        fi
    fi

    AC_MSG_CHECKING([will hwloc support be built])
    if test "$opal_hwloc_support" != "1"; then
        AC_MSG_WARN([HWLOC SUPPORT NOT FOUND])
        AC_MSG_ERROR([CANNOT CONTINUE])
    else
        AC_MSG_RESULT([yes])
    fi

    OPAL_SUMMARY_ADD([[Required Packages]],[[HWLOC]],[hwloc],[$opal_hwloc_dir])

    OPAL_VAR_SCOPE_POP
])dnl
