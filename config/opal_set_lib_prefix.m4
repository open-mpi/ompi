# -*- shell-script -*-
#
# Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
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
