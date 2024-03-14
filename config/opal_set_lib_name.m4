# -*- shell-script -*-
#
# Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_SET_LIB_PREFIX([library_prefix]
#
# This macro sets a name for the libopen-pal library.  Specifically,
# libopen-pal.la becomes libopen-pal.la by default.
#
# --------------------------------------------------------
AC_DEFUN([OPAL_SET_LIB_NAME],[
    AS_IF([test "$opal_lib_prefix_set" = "yes"],
          [AC_MSG_WARN([OPAL lib name was already set!])
           AC_MSG_WARN([This is a configury programming error])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_LIB_NAME=$1
    opal_lib_prefix_set=yes
    AC_SUBST(OPAL_LIB_NAME)
])dnl

#
# Rename 'libmpi' and 'libmpi_FOO' with a configure time option.
#
AC_DEFUN([OMPI_SET_LIB_NAME],[
    AC_MSG_CHECKING([if want custom libmpi(_FOO) name])
    AC_ARG_WITH([libmpi-name],
        [AS_HELP_STRING([--with-libmpi-name=STRING],
                ["Replace \"libmpi(_FOO)\" with \"libSTRING(_FOO)\" (default=mpi)"])])

    AS_IF([test "$with_libmpi_name" = "no"],
        [AC_MSG_RESULT([Error])
         AC_MSG_WARN([Invalid to specify --without-libmpi-name])
         AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$with_libmpi_name" = "" || test "$with_libmpi_name" = "yes"],
        [with_libmpi_name="mpi"])

    OMPI_LIBMPI_NAME=${with_libmpi_name}

    AC_MSG_RESULT([$OMPI_LIBMPI_NAME])
    AC_SUBST(OMPI_LIBMPI_NAME)
])dnl
