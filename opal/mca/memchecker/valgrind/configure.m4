# -*- shell-script -*-
#
# Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_memchecker_valgrind_PRIORITY], [10])

AC_DEFUN([MCA_opal_memchecker_valgrind_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memchecker_valgrind_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_memchecker_valgrind_CONFIG],[
    AC_CONFIG_FILES([opal/mca/memchecker/valgrind/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_memchecker_valgrind_save_CPPFLAGS opal_memchecker_valgrind_happy opal_memchecker_valgrind_CPPFLAGS])

    AC_ARG_WITH([valgrind],
        [AC_HELP_STRING([--with-valgrind(=DIR)],
            [Directory where the valgrind software is installed])])
    OPAL_CHECK_WITHDIR([valgrind], [$with_valgrind], [include/valgrind/valgrind.h])

    opal_memchecker_valgrind_CPPFLAGS=
    opal_memchecker_valgrind_save_CPPFLAGS="$CPPFLAGS"
    opal_memchecker_valgrind_happy=no
    AS_IF([test "$with_valgrind" != "no"],
          [AS_IF([test -n "$with_valgrind" && test "$with_valgrind" != "yes"],
                 [opal_memchecker_valgrind_CPPFLAGS="-I$with_valgrind/include"
                  # We need this -I to stay in CPPFLAGS when we're done
                  CPPFLAGS="$CPPFLAGS -I$with_valgrind/include"
                  opal_memchecker_valgrind_save_CPPFLAGS=$CPPFLAGS])
           AC_CHECK_HEADERS([valgrind/valgrind.h],
                 [AC_MSG_CHECKING([for VALGRIND_CHECK_MEM_IS_ADDRESSABLE])
                  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "valgrind/memcheck.h"
]],
                     [[char buffer = 0x0f;
                       VALGRIND_CHECK_MEM_IS_ADDRESSABLE(&buffer, sizeof(buffer));]])],
                     [AC_MSG_RESULT([yes])
                      opal_memchecker_valgrind_happy=yes],
                     [AC_MSG_RESULT([no])
                      AC_MSG_WARN([Need Valgrind version 3.2.0 or later. Can not build component.])]
                     [AC_MSG_RESULT([cross-compiling; assume yes...?])
                      AC_MSG_WARN([OMPI will fail to compile if you do not have Valgrind version 3.2.0 or later])
                      opal_memchecker_valgrind_happy=yes]),
                 ],
                 [AC_MSG_WARN([valgrind.h not found])
                  AC_MSG_WARN([Cannot compile this component])])])
    CPPFLAGS="$opal_memchecker_valgrind_save_CPPFLAGS"

    # If we specifically requested this component and can't build it, error
    AS_IF([test "$with_valgrind" != "no" && test -n "$with_valgrind" && test "$opal_memchecker_valgrind_happy" != "yes"],
          [AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$opal_memchecker_valgrind_happy" = "yes"],
          [$1],[$2])

    AC_SUBST([opal_memchecker_valgrind_CPPFLAGS])

    OPAL_VAR_SCOPE_POP
])dnl
