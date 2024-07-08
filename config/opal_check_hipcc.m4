dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2024      Stony Brook University.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl Check for HIPCC and bail out if HIPCC was requested
dnl Options provided:
dnl   --with-hipcc[=path/to/hipcc]: provide a path to HIPCC
dnl   --enable-hipcc: require HIPCC, bail out if not found
dnl

AC_DEFUN([OPAL_CHECK_HIPCC],[

    AC_ARG_ENABLE([hipcc],
        [AS_HELP_STRING([--enable-hipcc],
            [Force configure to fail if hipcc is not found (hipcc is used to build HIP operator support).])])

    AC_ARG_WITH([hipcc],
        [AS_HELP_STRING([--with-hipcc=DIR],
            [Path to the HIP compiler])])

    AS_IF([test -n "$with_hipcc"],
          [HIPCC=$with_hipcc])
    AS_IF([test -z "$HIPCC"],
          # try to find hipcc in PATH
          [AC_PATH_PROG([HIPCC], [hipcc], [])])

    # disable support if explicitly specified
    AS_IF([test "$enable_hipcc" = "no"],
          [HIPCC=])

    AS_IF([test -z "$HIPCC" && test "$enable_hipcc" = "yes"],
          [AC_MSG_WARN([A suitable HIP compiler was not found, but --enable-hipcc=yes was specified])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_SUMMARY_ADD([Accelerators], [HIPCC compiler], [], [$HIPCC (flags: $HIPCCFLAGS)])

    AC_ARG_VAR([HIPCC], [AMD HIP compiler])
    AC_ARG_VAR([HIPCCFLAGS], [AMD HIP compiler flags])

])
