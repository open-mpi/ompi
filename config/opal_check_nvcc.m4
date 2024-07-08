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

    AC_ARG_ENABLE([nvcc],
        [AS_HELP_STRING([--enable-nvcc],
            [Force configure to fail if CUDA nvcc is not found (CUDA nvcc is used to build CUDA operator support).])])

    AC_ARG_WITH([nvcc],
        [AS_HELP_STRING([--with-nvcc=DIR],
            [Path to the CUDA compiler])])

    AS_IF([test -n "$with_nvcc"],
          [NVCC=$with_nvcc])
    AS_IF([test -z "$NVCC"],
          # try to find nvcc in PATH
          [AC_PATH_PROG([NVCC], [nvcc], [])])

    # disable ussage of NVCC if explicitly specified
    AS_IF([test "$enable_nvcc" = "no"],
          [NVCC=])

    # prepend C++17 standard, allow override by user
    AS_IF([test -n "$NVCCFLAGS"],
          [NVCCFLAGS=--std c++17 $NVCCFLAGS],
          [NVCCFLAGS=--std c++17])

    AS_IF([test -z "$NVCC" && test "$enable_nvcc" = "yes"],
          [AC_MSG_WARN([A suitable CUDA compiler was not found, but --enable-nvcc=yes was specified])
           AC_MSG_ERROR([Cannot continue])])

    OPAL_SUMMARY_ADD([Accelerators], [NVCC compiler], [], [$NVCC (flags: $NVCCFLAGS)])

    AC_ARG_VAR([NVCC], [NVIDIA CUDA compiler])
    AC_ARG_VAR([NVCCFLAGS], [NVIDIA CUDA compiler flags])

])
