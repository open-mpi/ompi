dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl SPDX-License-Identifier: BSD-3-Clause-Open-MPI
dnl

AC_DEFUN([MCA_opal_memory_ucm_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_opal_memory_ucm_CONFIG([action-if-can-compile],
#                            [action-if-cant-compile])
# ------------------------------------------------
# Only the UCM headers are needed: the component finds the UCM library
# at run time, and only uses it if some other part of the process
# already loaded it.
AC_DEFUN([MCA_opal_memory_ucm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/memory/ucm/Makefile])

    OPAL_VAR_SCOPE_PUSH([memory_ucm_happy memory_ucm_CPPFLAGS_save])

    OMPI_CHECK_UCX([memory_ucm],
                   [memory_ucm_happy="yes"],
                   [memory_ucm_happy="no"])

    AS_IF([test "$memory_ucm_happy" = "yes"],
          [memory_ucm_CPPFLAGS_save="$CPPFLAGS"
           OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [$memory_ucm_CPPFLAGS])
           AC_CHECK_HEADERS([ucm/api/ucm.h], [], [memory_ucm_happy="no"])
           CPPFLAGS="$memory_ucm_CPPFLAGS_save"])

    AS_IF([test "$memory_ucm_happy" = "yes"], [$1], [$2])

    # Deliberately not substituting memory_ucm_LDFLAGS/LIBS: this
    # component must not add a dependency on the UCX libraries.
    AC_SUBST([memory_ucm_CPPFLAGS])

    OPAL_VAR_SCOPE_POP
])dnl
