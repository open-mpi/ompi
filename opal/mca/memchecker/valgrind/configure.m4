# -*- shell-script -*-
#
# Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_memchecker_valgrind_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memchecker_valgrind_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_memchecker_valgrind_CONFIG],[

    AC_ARG_WITH([valgrind],
        [AC_HELP_STRING([--with-valgrind(=DIR)],
            [Directory where the valgrind software is installed])])

    ompi_check_memchecker_valgrind_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_memchecker_valgrind_happy=no

    AS_IF([test "$with_valgrind" != "no"],
          [AS_IF([test ! -z "$with_valgrind" -a "$with_valgrind" != "yes"],
                 [CPPFLAGS="$CPPFLAGS -I$with_valgrind/include"
                  opal_memchecker_valgrind_CPPFLAGS="-I$with_valgrind/include"])
           AC_CHECK_HEADERS([valgrind/valgrind.h], 
                 [AC_MSG_CHECKING([for VALGRIND_CHECK_MEM_IS_ADDRESSABLE])
                  AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[
#include "valgrind/memcheck.h"
]],
                     [[char buffer = 0xff;
                       VALGRIND_CHECK_MEM_IS_ADDRESSABLE(&buffer, sizeof(buffer));]]),
                     [AC_MSG_RESULT([yes])
                      ompi_check_memchecker_valgrind_happy=yes],
                     [AC_MSG_RESULT([no])
                      AC_MSG_ERROR([Need Valgrind version 3.2.0 or later. Can not build component.])]
                     [AC_MSG_RESULT([cross-compiling; assume yes...?])
                      AC_MSG_WARN([OMPI will fail to compile if you do not have Valgrind version 3.2.0 or later])])
                 ],
                 [AC_MSG_WARN([valgrind.h not found])
                  AC_MSG_WARN([Cannot compile this component])])])

    CPPFLAGS="$ompi_check_memchecker_valgrind_save_CPPFLAGS"

    AS_IF([test "$ompi_check_valgrind_happy" = "yes"],
          [$1], [$2])
])dnl
