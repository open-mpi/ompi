# -*- shell-script -*-
#
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
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

    if test "$WANT_MEMCHECKER" = "1" ; then

        AC_ARG_WITH([valgrind],
            [AC_HELP_STRING([--with-valgrind(=DIR)],
                [Directory where the valgrind software is installed])])

        AC_MSG_CHECKING([checking for the valgrind include directory ])
        if test -n "$with_valgrind" -a -d "$with_valgrind/include" ; then
            CPPFLAGS="$CPPFLAGS -I$with_valgrind/include"
          AC_MSG_RESULT([$with_valgrind/include])
        else
          AC_MSG_RESULT([none needed])
        fi

        AC_CHECK_HEADER([valgrind/valgrind.h],
            [AC_CHECK_HEADER([valgrind/memcheck.h],
                [happy=yes])])

        if test "x$happy" != "xyes" ; then
            AC_MSG_WARN([*** Could not find valgrind header files, as valgrind support was requested])
            AC_MSG_ERROR([*** Cannot continue])
        fi

        AC_MSG_CHECKING([for VALGRIND_CHECK_MEM_IS_ADDRESSABLE in valgrind/memcheck.h])
        AC_TRY_COMPILE([#include "valgrind/memcheck.h"],
            [
            char buffer = 0xff;
            VALGRIND_CHECK_MEM_IS_ADDRESSABLE(&buffer, sizeof(buffer));
            ], valgrind_version_new=yes, valgrind_version_new=no)
        AC_MSG_RESULT($valgrind_version_new)

        if test "x$valgrind_version_new" != "xyes" ; then
            AC_MSG_WARN([*** Need at least version 3.2.0, please specify using --with-valgrind])
            AC_MSG_ERROR([*** Cannot continue])
        fi

    else
        happy=0                      # none_needed
        happy_value=0                # none_needed
        memchecker_valgrind_happy=0  # This should suffice to get rid of the component
        should_build=2
        want_component=0
    fi
])dnl
