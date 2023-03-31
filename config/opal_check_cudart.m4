dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2011-2015 NVIDIA Corporation.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OPAL_CHECK_CUDART(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if CUDA runtime library support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found

#
# Check for CUDA support
#
AC_DEFUN([OPAL_CHECK_CUDART],[
OPAL_VAR_SCOPE_PUSH([cudart_save_CPPFLAGS cudart_save_LDFLAGS cudart_save_LIBS])

cudart_save_CPPFLAGS="$CPPFLAGS"
cudart_save_LDFLAGS="$LDFLAGS"
cudart_save_LIBS="$LIBS"

#
# Check to see if the user provided paths for CUDART
#
AC_ARG_WITH([cudart],
            [AS_HELP_STRING([--with-cudart=DIR],
            [Path to the CUDA runtime library and header files])])
AC_MSG_CHECKING([if --with-cudart is set])
AC_ARG_WITH([cudart-libdir],
            [AS_HELP_STRING([--with-cudart-libdir=DIR],
                            [Search for CUDA runtime libraries in DIR])])

####################################
#### Check for CUDA runtime library
####################################
AS_IF([test "x$with_cudart" != "xno" || test "x$with_cudart" = "x"],
      [opal_check_cudart_happy=no
       AC_MSG_RESULT([not set (--with-cudart=$with_cudart)])],
      [AS_IF([test ! -d "$with_cudart"],
             [AC_MSG_RESULT([not found])
              AC_MSG_WARN([Directory $with_cudart not found])]
             [AS_IF([test "x`ls $with_cudart/include/cuda_runtime.h 2> /dev/null`" = "x"]
                    [AC_MSG_RESULT([not found])
                     AC_MSG_WARN([Could not find cuda_runtime.h in $with_cudart/include])]
                    [opal_check_cudart_happy=yes
                     opal_cudart_incdir="$with_cudart/include"])])])

AS_IF([test "$opal_check_cudart_happy" = "no" && test "$with_cudart" != "no"],
      [AC_PATH_PROG([nvcc_bin], [nvcc], ["not-found"])
       AS_IF([test "$nvcc_bin" = "not-found"],
             [AC_MSG_WARN([Could not find nvcc binary])],
             [nvcc_dirname=`AS_DIRNAME([$nvcc_bin])`
              with_cudart=$nvcc_dirname/../
              opal_cudart_incdir=$nvcc_dirname/../include
              opal_check_cudart_happy=yes])
      ]
      [])

AS_IF([test x"$with_cudart_libdir" = "x"],
      [with_cudart_libdir=$with_cudart/lib64/]
      [])

AS_IF([test "$opal_check_cudart_happy" = "yes"],
    [OAC_CHECK_PACKAGE([cudart],
                       [$1],
                       [cuda_runtime.h],
                       [cudart],
                       [cudaMalloc],
                       [opal_check_cudart_happy="yes"],
                       [opal_check_cudart_happy="no"])],
    [])


AC_MSG_CHECKING([if have cuda runtime library support])
if test "$opal_check_cudart_happy" = "yes"; then
    AC_MSG_RESULT([yes (-I$opal_cudart_incdir)])
    CUDART_SUPPORT=1
    common_cudart_CPPFLAGS="-I$opal_cudart_incdir"
    AC_SUBST([common_cudart_CPPFLAGS])
else
    AC_MSG_RESULT([no])
    CUDART_SUPPORT=0
fi


OPAL_SUMMARY_ADD([Accelerators], [CUDART support], [], [$opal_check_cudart_happy])
AM_CONDITIONAL([OPAL_cudart_support], [test "x$CUDART_SUPPORT" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDART_SUPPORT],$CUDART_SUPPORT,
                   [Whether we have cuda runtime library support])

CPPFLAGS=${cudart_save_CPPFLAGS}
LDFLAGS=${cudart_save_LDFLAGS}
LIBS=${cudart_save_LIBS}
OPAL_VAR_SCOPE_POP
])dnl
