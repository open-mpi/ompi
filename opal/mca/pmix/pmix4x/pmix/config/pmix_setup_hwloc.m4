# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([PMIX_HWLOC_CONFIG],[
    PMIX_VAR_SCOPE_PUSH([pmix_hwloc_dir pmix_hwloc_libdir pmix_hwloc_standard_lib_location pmix_hwloc_standard_header_location])

    AC_ARG_WITH([hwloc],
                [AC_HELP_STRING([--with-hwloc=DIR],
                                [Search for hwloc headers and libraries in DIR ])])

    AC_ARG_WITH([hwloc-libdir],
                [AC_HELP_STRING([--with-hwloc-libdir=DIR],
                                [Search for hwloc libraries in DIR ])])

    pmix_hwloc_support=0
    AS_IF([test "$with_hwloc" = "internal" || test "$with_hwloc" = "external"],
          [with_hwloc=])

    if test "$with_hwloc" != "no"; then
        AC_MSG_CHECKING([for hwloc in])
        if test ! -z "$with_hwloc" && test "$with_hwloc" != "yes"; then
            pmix_hwloc_dir=$with_hwloc
            pmix_hwloc_standard_header_location=no
            pmix_hwloc_standard_lib_location=no
            AS_IF([test -z "$with_hwloc_libdir" || test "$with_hwloc_libdir" = "yes"],
                  [if test -d $with_hwloc/lib; then
                       pmix_hwloc_libdir=$with_hwloc/lib
                   elif test -d $with_hwloc/lib64; then
                       pmix_hwloc_libdir=$with_hwloc/lib64
                   else
                       AC_MSG_RESULT([Could not find $with_hwloc/lib or $with_hwloc/lib64])
                       AC_MSG_ERROR([Can not continue])
                   fi
                   AC_MSG_RESULT([$pmix_hwloc_dir and $pmix_hwloc_libdir])],
                  [AC_MSG_RESULT([$with_hwloc_libdir])])
        else
            AC_MSG_RESULT([(default search paths)])
            pmix_hwloc_standard_header_location=yes
            pmix_hwloc_standard_lib_location=yes
        fi
        AS_IF([test ! -z "$with_hwloc_libdir" && test "$with_hwloc_libdir" != "yes"],
              [pmix_hwloc_libdir="$with_hwloc_libdir"
               pmix_hwloc_standard_lib_location=no])

        PMIX_CHECK_PACKAGE([pmix_hwloc],
                           [hwloc.h],
                           [hwloc],
                           [hwloc_topology_init],
                           [-lhwloc],
                           [$pmix_hwloc_dir],
                           [$pmix_hwloc_libdir],
                           [pmix_hwloc_support=1],
                           [pmix_hwloc_support=0])
        if test $pmix_hwloc_support = "1"; then
            LIBS="$LIBS -lhwloc"
            PMIX_EMBEDDED_LIBS="$PMIX_EMBEDDED_LIBS -lhwloc"
            if test "$pmix_hwloc_standard_header_location" != "yes"; then
                PMIX_EMBEDDED_CPPFLAGS="$PMIX_EMBEDDED_CPPFLAGS $pmix_hwloc_CPPFLAGS"
                CPPFLAGS="$CPPFLAGS $pmix_hwloc_CPPFLAGS"
            fi
            if test "$pmix_hwloc_standard_lib_location" != "yes"; then
                PMIX_EMBEDDED_LDFLAGS="$PMIX_EMBEDDED_LDFLAGS $pmix_hwloc_LDFLAGS"
                LDFLAGS="$LDFLAGS $pmix_hwloc_LDFLAGS"
            fi
        fi
    fi

    if test ! -z "$with_hwloc" && test "$with_hwloc" != "no" && test "$pmix_hwloc_support" != "1"; then
        AC_MSG_WARN([HWLOC SUPPORT REQUESTED AND NOT FOUND])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    if test $pmix_hwloc_support = "1"; then
        AC_MSG_CHECKING([if external hwloc version is 1.5 or greater])
        AC_COMPILE_IFELSE(
              [AC_LANG_PROGRAM([[#include <hwloc.h>]],
              [[
    #if HWLOC_API_VERSION < 0x00010500
    #error "hwloc API version is less than 0x00010500"
    #endif
              ]])],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_RESULT([no])
               AC_MSG_ERROR([Cannot continue])])
    fi

    AC_MSG_CHECKING([will hwloc support be built])
    if test "$pmix_hwloc_support" != "1"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([yes])
    fi

    AC_DEFINE_UNQUOTED([PMIX_HAVE_HWLOC], [$pmix_hwloc_support],
                       [Whether or not we have hwloc support])
    PMIX_VAR_SCOPE_POP
])dnl
