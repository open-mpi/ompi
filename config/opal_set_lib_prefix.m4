# -*- shell-script -*-
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_SET_LIB_PREFIX([library_prefix]
#
# This macro sets a prefix for the libopen-pal library.  Specifically,
# libopen-pal.la becomes libPREFIXopen-pal.la.
#
# --------------------------------------------------------
AC_DEFUN([OPAL_SET_LIB_PREFIX],[
    AS_IF([test "$opal_lib_prefix_set" = "yes"],
          [AC_MSG_WARN([OPAL lib prefix was already set!])
           AC_MSG_WARN([This is a configury programming error])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_LIB_PREFIX=$1
    opal_lib_prefix_set=yes
    AC_SUBST(OPAL_LIB_PREFIX)
])dnl

#
# Same as OPAL LIB_PREFIX, but for the ORTE layer
#
AC_DEFUN([ORTE_SET_LIB_PREFIX],[
    AS_IF([test "$orte_lib_prefix_set" = "yes"],
          [AC_MSG_WARN([ORTE lib prefix was already set!])
           AC_MSG_WARN([This is a configury programming error])
           AC_MSG_ERROR([Cannot continue])])

    ORTE_LIB_PREFIX=$1
    orte_lib_prefix_set=yes
    AC_SUBST(ORTE_LIB_PREFIX)
])dnl

#
# Rename 'libmpi' and 'libmpi_FOO' with a configure time option.
#
AC_DEFUN([OMPI_SET_LIB_NAME],[
    AC_MSG_CHECKING([if want custom libmpi(_FOO) name])
    AC_ARG_WITH([libmpi-name],
        [AC_HELP_STRING([--with-libmpi-name=STRING],
                ["Replace \"libmpi(_FOO)\" with \"libSTRING(_FOO)\" (default=mpi)"])])

    AS_IF([test "$with_libmpi_name" = "no"],
        [AC_MSG_RESULT([Error])
         AC_MSG_WARN([Invalid to specify --without-libmpi-name])
         AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$with_libmpi_name" = "" || test "$with_libmpi_name" = "yes"],
        [with_libmpi_name="mpi"])

    AC_MSG_RESULT([$with_libmpi_name])
    AC_SUBST(OMPI_LIBMPI_NAME, $with_libmpi_name)
])dnl
