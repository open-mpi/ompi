# -*- autocont -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Research Organization for Information Science
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
    OPAL_DECLARE_PACKAGE([zlib],
                         [Search for zlib headers and libraries in DIR],
                         [Search for zlib headers with these CPPFLAGS],
                         [Search for zlib library with these LDFLAGS])

    OPAL_CHECK_PACKAGE2([zlib],
                        [opal_zlib],
                        [zlib.h],
                        [],
                        [deflate],
                        [z],
                        [],
                        [opal_zlib_support=1],
                        [opal_zlib_support=0])

    AS_IF([test $opal_zlib_support -eq 1],
          [LIBS="$LIBS -lz"
           CPPFLAGS="$CPPFLAGS $opal_zlib_CPPFLAGS"
           LDFLAGS="$LDFLAGS $opal_zlib_LDFLAGS"])

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
])dnl
