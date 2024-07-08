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
dnl Check for NVCC and bail out if NVCC was requested
dnl Options provided:
dnl   --with-nvcc[=path/to/nvcc]: provide a path to NVCC
dnl   --enable-nvcc: require NVCC, bail out if not found
dnl   --nvcc-compute-arch: request a specific compute
dnl                        architecture for the operator
dnl                        kernels
dnl

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
