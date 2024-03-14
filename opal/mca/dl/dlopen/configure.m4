# -*- shell-script -*-
#
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_dl_dlopen_PRIORITY], [80])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_dl_dlopen_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_dl_dlopen_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_dl_dlopen_CONFIG],[
    AC_CONFIG_FILES([opal/mca/dl/dlopen/Makefile])

    OAC_CHECK_PACKAGE([dlopen],
              [opal_dl_dlopen],
              [dlfcn.h],
              [dl],
              [dlopen],
              [opal_dl_dlopen_happy=yes],
              [opal_dl_dlopen_happy=no])

    AS_IF([test "$opal_dl_dlopen_happy" = "yes"],
          [$1],
          [$2])

    AC_SUBST(opal_dl_dlopen_LIBS)
])
