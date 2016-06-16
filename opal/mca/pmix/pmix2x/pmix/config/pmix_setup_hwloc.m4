# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([PMIX_HWLOC_CONFIG],[
    AC_ARG_WITH([hwloc-header],
                [AC_HELP_STRING([--with-hwloc-header=HEADER],
                                [The value that should be included in C files to include hwloc.h])])

    pmix_hwloc_support=0
    AS_IF([test "$enable_embedded_mode" = "yes"],
          [_PMIX_HWLOC_EMBEDDED_MODE],
          [_PMIX_HWLOC_EXTERNAL])

    AC_DEFINE_UNQUOTED(PMIX_HAVE_HWLOC, [$pmix_hwloc_support],
                      [Whether we have hwloc support or not])

    AC_MSG_CHECKING([hwloc header])
    AC_DEFINE_UNQUOTED([PMIX_HWLOC_HEADER], [$PMIX_HWLOC_HEADER],
                       [Location of hwloc.h])
    AC_MSG_RESULT([$PMIX_HWLOC_HEADER])

    CPPFLAGS="$CPPFLAGS $PMIX_HWLOC_CPPFLAGS"
    LDFLAGS="$LDFLAGS $PMIX_HWLOC_LDFLAGS"
    LIBS="$LIBS $PMIX_HWLOC_LIBS"
])

AC_DEFUN([_PMIX_HWLOC_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for hwloc])
    AC_MSG_RESULT([assumed available (embedded mode)])

    PMIX_HWLOC_HEADER="$with_hwloc_header"
    PMIX_HWLOC_CPPFLAGS=
    PMIX_HWLOC_LIB=
    PMIX_HWLOC_LDFLAGS=

    pmix_hwloc_support=1
])

AC_DEFUN([_PMIX_HWLOC_EXTERNAL],[
    PMIX_VAR_SCOPE_PUSH([pmix_hwloc_dir pmix_hwloc_libdir])

    AC_ARG_WITH([hwloc],
                [AC_HELP_STRING([--with-hwloc=DIR],
                                [Search for hwloc headers and libraries in DIR ])])

    AC_ARG_WITH([hwloc-libdir],
                [AC_HELP_STRING([--with-hwloc-libdir=DIR],
                                [Search for hwloc libraries in DIR ])])

    pmix_hwloc_support=0
    if test "$with_hwloc" != "no"; then
        AC_MSG_CHECKING([for hwloc in])
        if test ! -z "$with_hwloc" && test "$with_hwloc" != "yes"; then
            pmix_hwloc_dir=$with_hwloc
            if test -d $with_hwloc/lib; then
                pmix_hwloc_libdir=$with_hwloc/lib
            elif test -d $with_hwloc/lib64; then
                pmix_hwloc_libdir=$with_hwloc/lib64
            else
                AC_MSG_RESULT([Could not find $with_hwloc/lib or $with_hwloc/lib64])
                AC_MSG_ERROR([Can not continue])
            fi
            AC_MSG_RESULT([$pmix_hwloc_dir and $pmix_hwloc_libdir])
        else
            AC_MSG_RESULT([(default search paths)])
        fi
        AS_IF([test ! -z "$with_hwloc_libdir" && test "$with_hwloc_libdir" != "yes"],
              [pmix_hwloc_libdir="$with_hwloc_libdir"])

        PMIX_CHECK_PACKAGE([pmix_hwloc],
                           [hwloc.h],
                           [hwloc],
                           [hwloc_topology_dup],
                           [-lhwloc],
                           [$pmix_hwloc_dir],
                           [$pmix_hwloc_libdir],
                           [pmix_hwloc_support=1],
                           [pmix_hwloc_support=0])
        if test $pmix_hwloc_support == "1"; then
            CPPFLAGS="$pmix_hwloc_CPPFLAGS $CPPFLAGS"
            LIBS="$LIBS -lhwloc"
            LDFLAGS="$pmix_hwloc_LDFLAGS $LDFLAGS"
        fi
    fi

    if test ! -z "$with_hwloc" && test "$with_hwloc" != "no" && test "$pmix_hwloc_support" != "1"; then
        AC_MSG_WARN([HWLOC SUPPORT REQUESTED AND NOT FOUND. PMIX HWLOC])
        AC_MSG_WARN([SUPPORT REQUIRES A MINIMUM OF VERSION 1.9.1])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    # Set output variables
    PMIX_HWLOC_HEADER="<hwloc.h>"
    PMIX_HWLOC_LIB=-lhwloc
    AS_IF([test "$pmix_hwloc_dir" != ""],
        [PMIX_HWLOC_CPPFLAGS="-I$pmix_hwloc_dir/include"])
    AS_IF([test "$pmix_hwloc_libdir" != ""],
        [PMIX_HWLOC_LDFLAGS="-L$pmix_hwloc_libdir"])

    AC_MSG_CHECKING([will hwloc support be built])
    if test "$pmix_hwloc_support" != "1"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([yes])
    fi

    PMIX_VAR_SCOPE_POP
])dnl
