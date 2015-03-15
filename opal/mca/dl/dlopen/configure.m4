# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
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

    dnl This is effectively a back-door for Open MPI developers to
    dnl force the use of the libltdl dl component.
    AC_ARG_ENABLE([dl-dlopen],
        [AS_HELP_STRING([--disable-dl-dlopen],
            [Disable the "dlopen" DL component (and probably force the use of the "libltdl" DL component).  This option should really only be used by Open MPI developers.  You are probably actually looking for the "--disable-dlopen" option, which disables all dlopen-like functionality from Open MPI.])
        ])

    opal_dl_dlopen_happy=no
    AS_IF([test "$enable_dl_dlopen" != "no"],
          [OPAL_CHECK_PACKAGE([opal_dl_dlopen],
              [dlfcn.h],
              [dl],
              [dlopen],
              [],
              [],
              [],
              [opal_dl_dlopen_happy=yes],
              [opal_dl_dlopen_happy=no])
          ])

    AS_IF([test "$opal_dl_dlopen_happy" = "yes"],
          [opal_dl_dlopen_ADD_LIBS=$opal_dl_dlopen_LIBS
           $1],
          [$2])

    AC_SUBST(opal_dl_dlopen_LIBS)
])
