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

    # This option is probably only helpful to developers: have
    # configure fail if Sphinx is not found (i.e., if you don't have
    # the ability to use Sphinx to build the HTML docs and man pages).
    AC_ARG_ENABLE([hipcc],
        [AS_HELP_STRING([--enable-hipcc],
            [Force configure to fail if CUDA hipcc is not found (CUDA hipcc is used to build CUDA operator support).])])

    AC_ARG_WITH([hipcc],
        [AS_HELP_STRING([--with-hipcc=DIR],
            [Path to the CUDA compiler])])

    AS_IF([test -n "$with_hipcc"],
          [OPAL_HIPCC=$with_hipcc],
          # no path specified, try to find hipcc
          [AC_PATH_PROG([OPAL_HIPCC], [hipcc], [])])

    # If the user requested to disable sphinx, then pretend we didn't
    # find it.
    AS_IF([test "$enable_hipcc" = "no"],
          [OPAL_HIPCC=])

    # If --enable-sphinx was specified and we did not find Sphinx,
    # abort.  This is likely only useful to prevent "oops!" moments
    # from developers.
    AS_IF([test -z "$OPAL_HIPCC" && test "$enable_hipcc" = "yes"],
          [AC_MSG_WARN([A suitable CUDA compiler was not found, but --enable-hipcc was specified])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_SUMMARY_ADD([Accelerators], [HIPCC compiler], [], [$OPAL_HIPCC])

])
