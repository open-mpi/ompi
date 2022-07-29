# -*- shell-script -*-
#
# Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
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
        [AS_HELP_STRING([--with-valgrind(=DIR)],
            [Directory where the valgrind software is installed])])

    OAC_CHECK_PACKAGE([valgrind],
                      [memchecker_valgrind],
                      [valgrind/valgrind.h],
                      [],
                      [],
                      [opal_memchecker_valgrind_happy=yes],
                      [opal_memchecker_valgrind_happy=no])

    # We only need the headers for Valgrind; ignore the libraries if
    # we used the module.  Ignore the pkg-config module to avoid
    # leaking libraries into the pkg-config module later.
    AS_UNSET([memchecker_valgrind_LDFLAGS])
    AS_UNSET([memchecker_valgrind_LIBS])
    AS_UNSET([memchecker_valgrind_PC_MODULES])

    AS_IF([test "${opal_memchecker_valgrind_happy}" = "yes"],
          [opal_memchecker_valgrind_save_CPPFLAGS=${CPPFLAGS}
           OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${memchecker_valgrind_CPPFLAGS}])

           AC_CACHE_CHECK([for VALGRIND_CHECK_MEM_IS_ADDRESSABLE],
              [opal_memchecker_valgrind_cv_check_mem_is_addressable],
              [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "valgrind/memcheck.h"
                  ]], [[
char buffer = 0x0f;
VALGRIND_CHECK_MEM_IS_ADDRESSABLE(&buffer, sizeof(buffer));
                  ]])],
                 [opal_memchecker_valgrind_cv_check_mem_is_addressable=yes],
                 [opal_memchecker_valgrind_cv_check_mem_is_addressable=no],
                 [opal_memchecker_valgrind_cv_check_mem_is_addressable=cross-compiling])])

           CPPFLAGS=${opal_memchecker_valgrind_save_CPPFLAGS}

           AS_IF([test "${opal_memchecker_valgrind_cv_check_mem_is_addressable}" = "no"],
                 [AC_MSG_WARN([Need Valgrind version 3.2.0 or later. Can not build component.])
                  opal_memchecker_valgrind_happy=no],
                 [test "${opal_memchecker_valgrind_cv_check_mem_is_addressable}" = "cross-compiling"],
                 [AC_MSG_WARN([OMPI will fail to compile if you do not have Valgrind version 3.2.0 or later])])])

    # If we specifically requested this component and can't build it, error
    AS_IF([test "$with_valgrind" != "no" && test -n "$with_valgrind" && test "$opal_memchecker_valgrind_happy" != "yes"],
          [AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$opal_memchecker_valgrind_happy" = "yes"],
          [$1],[$2])

    AC_SUBST([memchecker_valgrind_CPPFLAGS])

    OPAL_VAR_SCOPE_POP
])dnl
