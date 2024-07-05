dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2020-2022 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2024 Jeffrey M. Squyres.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Setup Sphinx for building HTML docs and man pages
dnl
dnl 1 -> sanity file to check if pre-built docs are already available
dnl      You probably want to pass something like
dnl      "$srcdir/docs/_build/man/foo.1"
dnl
dnl 2 -> (OPTIONAL) URL to display in AC_MSG_WARN when docs will not be installed
dnl      If $2 is empty, nothing will be displayed.
dnl      Note: if $2 contains a #, be sure to double quote it
dnl      (e.g., [[https://example.com/foo.html#some-anchor]])
dnl
dnl 3 -> (OPTIONAL) Filename of requirements.txt-like file containing
dnl      the required Pip modules (to be displayed if rendering a
dnl      simple RST project fails).
dnl
dnl This macro requires that OAC_PUSH_PREFIX was previously called.
dnl The pushed prefix may be used if this macro chooses to set {OAC
dnl prefix}_MAKEDIST_DISABLE.  If set, it is a message indicating why
dnl "make dist" should be disabled, suitable for emitting via
dnl AC_MSG_WARN.
AC_DEFUN([OPAL_CHECK_NVCC],[

    # This option is probably only helpful to developers: have
    # configure fail if Sphinx is not found (i.e., if you don't have
    # the ability to use Sphinx to build the HTML docs and man pages).
    AC_ARG_ENABLE([nvcc],
        [AS_HELP_STRING([--enable-nvcc],
            [Force configure to fail if CUDA nvcc is not found (CUDA nvcc is used to build CUDA operator support).])])

    AC_ARG_WITH([nvcc],
        [AS_HELP_STRING([--with-nvcc=DIR],
            [Path to the CUDA compiler])])

    AC_ARG_WITH([nvcc_compute_arch],
        [AS_HELP_STRING([--with-nvcc-compute-arch=ARCH],
            [Compute architecture to use for CUDA (default: 52)])])

    AS_IF([test -n "$with_nvcc"],
          [OPAL_NVCC=$with_nvcc],
          # no path specified, try to find nvcc
          [AC_PATH_PROG([OPAL_NVCC], [nvcc], [])])

    # If the user requested to disable sphinx, then pretend we didn't
    # find it.
    AS_IF([test "$enable_nvcc" = "no"],
          [OPAL_NVCC=])

    # default to CUDA compute architecture 52
    AS_IF([test -n "$with_nvcc_compute_arch"],
          [OPAL_NVCC_COMPUTE_ARCH=$with_nvcc_compute_arch],
          [OPAL_NVCC_COMPUTE_ARCH=52])

    # If --enable-sphinx was specified and we did not find Sphinx,
    # abort.  This is likely only useful to prevent "oops!" moments
    # from developers.
    AS_IF([test -z "$OPAL_NVCC" && test "$enable_nvcc" = "yes"],
          [AC_MSG_WARN([A suitable CUDA compiler was not found, but --enable-nvcc was specified])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_SUMMARY_ADD([Accelerators], [NVCC compiler], [], [$OPAL_NVCC (compute arch: $OPAL_NVCC_COMPUTE_ARCH)])

])
