# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pcompress_zlib_CONFIG([action-if-can-compile],
#                           [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pcompress_zlib_CONFIG],[
    AC_CONFIG_FILES([src/mca/pcompress/zlib/Makefile])

    PMIX_VAR_SCOPE_PUSH([pmix_zlib_dir pmix_zlib_libdir pmix_zlib_standard_lib_location pmix_zlib_standard_header_location pmix_check_zlib_save_CPPFLAGS pmix_check_zlib_save_LDFLAGS pmix_check_zlib_save_LIBS])

    AC_ARG_WITH([zlib],
                [AC_HELP_STRING([--with-zlib=DIR],
                                [Search for zlib headers and libraries in DIR ])])

    AC_ARG_WITH([zlib-libdir],
                [AC_HELP_STRING([--with-zlib-libdir=DIR],
                                [Search for zlib libraries in DIR ])])

    pmix_check_zlib_save_CPPFLAGS="$CPPFLAGS"
    pmix_check_zlib_save_LDFLAGS="$LDFLAGS"
    pmix_check_zlib_save_LIBS="$LIBS"

    pmix_zlib_support=0

    if test "$with_zlib" != "no"; then
        AC_MSG_CHECKING([for zlib in])
        if test ! -z "$with_zlib" && test "$with_zlib" != "yes"; then
            pmix_zlib_dir=$with_zlib
            pmix_zlib_source=$with_zlib
            pmix_zlib_standard_header_location=no
            pmix_zlib_standard_lib_location=no
            AS_IF([test -z "$with_zlib_libdir" || test "$with_zlib_libdir" = "yes"],
                  [if test -d $with_zlib/lib; then
                       pmix_zlib_libdir=$with_zlib/lib
                   elif test -d $with_zlib/lib64; then
                       pmix_zlib_libdir=$with_zlib/lib64
                   else
                       AC_MSG_RESULT([Could not find $with_zlib/lib or $with_zlib/lib64])
                       AC_MSG_ERROR([Can not continue])
                   fi
                   AC_MSG_RESULT([$pmix_zlib_dir and $pmix_zlib_libdir])],
                  [AC_MSG_RESULT([$with_zlib_libdir])])
        else
            AC_MSG_RESULT([(default search paths)])
            pmix_zlib_source=standard
            pmix_zlib_standard_header_location=yes
            pmix_zlib_standard_lib_location=yes
        fi
        AS_IF([test ! -z "$with_zlib_libdir" && test "$with_zlib_libdir" != "yes"],
              [pmix_zlib_libdir="$with_zlib_libdir"
               pmix_zlib_standard_lib_location=no])

        PMIX_CHECK_PACKAGE([pcompress_zlib],
                           [zlib.h],
                           [z],
                           [deflate],
                           [-lz],
                           [$pmix_zlib_dir],
                           [$pmix_zlib_libdir],
                           [pmix_zlib_support=1],
                           [pmix_zlib_support=0])
    fi

    if test ! -z "$with_zlib" && test "$with_zlib" != "no" && test "$pmix_zlib_support" != "1"; then
        AC_MSG_WARN([ZLIB SUPPORT REQUESTED AND NOT FOUND])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will zlib support be built])
    if test "$pmix_zlib_support" != "1"; then
        AC_MSG_RESULT([no])
    else
        AC_MSG_RESULT([yes])
    fi

    CPPFLAGS="$pmix_check_zlib_save_CPPFLAGS"
    LDFLAGS="$pmix_check_zlib_save_LDFLAGS"
    LIBS="$pmix_check_zlib_save_LIBS"

    AS_IF([test "$pmix_zlib_support" = "1"],
          [$1
           pcompress_zlib_LIBS=-lz
           PMIX_SUMMARY_ADD([[External Packages]],[[ZLIB]], [pmix_zlib], [yes ($pmix_zlib_source)])],
          [$2])

    # substitute in the things needed to build pcompress/zlib
    AC_SUBST([pcompress_zlib_CFLAGS])
    AC_SUBST([pcompress_zlib_CPPFLAGS])
    AC_SUBST([pcompress_zlib_LDFLAGS])
    AC_SUBST([pcompress_zlib_LIBS])

    PMIX_EMBEDDED_LIBS="$PMIX_EMBEDDED_LIBS $pcompress_zlib_LIBS"
    PMIX_EMBEDDED_LDFLAGS="$PMIX_EMBEDDED_LDFLAGS $pcompress_zlib_LDFLAGS"
    PMIX_EMBEDDED_CPPFLAGS="$PMIX_EMBEDDED_CPPFLAGS $pcompress_zlib_CPPFLAGS"

    PMIX_VAR_SCOPE_POP
])dnl
