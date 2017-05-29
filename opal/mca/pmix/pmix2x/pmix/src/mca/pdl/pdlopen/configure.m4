# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# Copyright (c) 2017      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_pmix_pdl_pdlopen_PRIORITY], [80])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_pmix_pdl_pdlopen_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $1:$2 compile mode])
    $3="static"
    AC_MSG_RESULT([$$3])
])

# MCA_pmix_pdl_pdlopen_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_pmix_pdl_pdlopen_POST_CONFIG],[
    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"],
          [
           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           LDFLAGS="$LDFLAGS $pmix_pdl_pdlopen_ADD_LDFLAGS"
           LIBS="$LIBS $pmix_pdl_pdlopen_ADD_LIBS"
          ])
])dnl

# MCA_pdl_pdlopen_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pdl_pdlopen_CONFIG],[
    AC_CONFIG_FILES([src/mca/pdl/pdlopen/Makefile])

    dnl This is effectively a back-door for PMIX developers to
    dnl force the use of the libltdl pdl component.
    AC_ARG_ENABLE([dl-dlopen],
        [AS_HELP_STRING([--disable-dl-dlopen],
                        [Disable the "dlopen" PDL component (and probably force the use of the "libltdl" PDL component).])
        ])

    pmix_pdl_pdlopen_happy=no
    AS_IF([test "$enable_dl_dlopen" != "no"],
          [PMIX_CHECK_PACKAGE([pmix_pdl_pdlopen],
              [dlfcn.h],
              [dl],
              [dlopen],
              [],
              [],
              [],
              [pmix_pdl_pdlopen_happy=yes],
              [pmix_pdl_pdlopen_happy=no])
          ])

    AS_IF([test "$pmix_pdl_pdlopen_happy" = "yes"],
          [pmix_pdl_pdlopen_ADD_LIBS=$pmix_pdl_pdlopen_LIBS
           $1],
          [$2])

    AC_SUBST(pmix_pdl_pdlopen_LIBS)
])
