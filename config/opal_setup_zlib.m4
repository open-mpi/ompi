# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_zlib_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([OPAL_ZLIB_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_zlib_dir opal_zlib_libdir opal_zlib_standard_header_location opal_zlib_standard_lib_location])

    AC_ARG_WITH([zlib],
                [AC_HELP_STRING([--with-zlib=DIR],
                                [Search for zlib headers and libraries in DIR ])])

    AC_ARG_WITH([zlib-libdir],
                [AC_HELP_STRING([--with-zlib-libdir=DIR],
                                [Search for zlib libraries in DIR ])])

    opal_zlib_support=0
    if test "$with_zlib" != "no"; then
        AC_MSG_CHECKING([for zlib in])
        if test ! -z "$with_zlib" && test "$with_zlib" != "yes"; then
            opal_zlib_dir=$with_zlib
            opal_zlib_standard_header_location=no
            opal_zlib_standard_lib_location=no
            AS_IF([test -z "$with_zlib_libdir" || test "$with_zlib_libdir" = "yes"],
                  [if test -d $with_zlib/lib; then
                       opal_zlib_libdir=$with_zlib/lib
                   elif test -d $with_zlib/lib64; then
                       opal_zlib_libdir=$with_zlib/lib64
                   else
                       AC_MSG_RESULT([Could not find $with_zlib/lib or $with_zlib/lib64])
                       AC_MSG_ERROR([Can not continue])
                   fi
                   AC_MSG_RESULT([$opal_zlib_dir and $opal_zlib_libdir])],
                  [AC_MSG_RESULT([$with_zlib_libdir])])
        else
            AC_MSG_RESULT([(default search paths)])
            opal_zlib_standard_header_location=yes
            opal_zlib_standard_lib_location=yes
        fi
        AS_IF([test ! -z "$with_zlib_libdir" && test "$with_zlib_libdir" != "yes"],
              [opal_zlib_libdir="$with_zlib_libdir"
               opal_zlib_standard_lib_location=no])

        OPAL_CHECK_PACKAGE([opal_zlib],
                           [zlib.h],
                           [z],
                           [deflate],
                           [-lz],
                           [$opal_zlib_dir],
                           [$opal_zlib_libdir],
                           [opal_zlib_support=1],
                           [opal_zlib_support=0])
        if test $opal_zlib_support = "1"; then
            LIBS="$LIBS -lz"
            if test "$opal_zlib_standard_header_location" != "yes"; then
                CPPFLAGS="$CPPFLAGS $opal_zlib_CPPFLAGS"
            fi
            if test "$opal_zlib_standard_lib_location" != "yes"; then
                LDFLAGS="$LDFLAGS $opal_zlib_LDFLAGS"
            fi
        fi
    fi

    if test ! -z "$with_zlib" && test "$with_zlib" != "no" && test "$opal_zlib_support" != "1"; then
        AC_MSG_WARN([ZLIB SUPPORT REQUESTED AND NOT FOUND])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will zlib support be built])
    if test "$opal_zlib_support" != "1"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([yes])
    fi

    AC_DEFINE_UNQUOTED([OPAL_HAVE_ZLIB], [$opal_zlib_support],
                       [Whether or not we have zlib support])
    OPAL_VAR_SCOPE_POP
])dnl
