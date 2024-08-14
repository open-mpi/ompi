dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
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


# OPAL_CHECK_CUDA(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if CUDA support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found

#
# Check for CUDA support
#
AC_DEFUN([OPAL_CHECK_CUDA],[
OPAL_VAR_SCOPE_PUSH([cuda_save_CPPFLAGS cuda_save_LDFLAGS cuda_save_LIBS])

cuda_save_CPPFLAGS="$CPPFLAGS"
cuda_save_LDFLAGS="$LDFLAGS"
cuda_save_LIBS="$LIBS"
#
# Check to see if user wants CUDA support
#
AC_ARG_WITH([cuda],
            [AS_HELP_STRING([--with-cuda(=DIR)],
            [Build cuda support, optionally adding DIR/include])])
AC_MSG_CHECKING([if --with-cuda is set])

# Search for libcuda.so in $with_cuda if the user didn't pass --with-cuda-libdir
# Otherwise check for cuda in the default path, /usr/local/cuda. If the default
# path doesn't exist, set with_cuda_libdir to empty.
AC_ARG_WITH([cuda-libdir],
            [AS_HELP_STRING([--with-cuda-libdir=DIR],
                            [Search for CUDA libraries in DIR])],
            [],
            [AS_IF([test -d "$with_cuda"],
             [with_cuda_libdir=$(dirname $(find -H $with_cuda -name libcuda.so 2> /dev/null) 2> /dev/null)],
             [with_cuda_libdir=$(dirname $(find -H /usr/local/cuda -name libcuda.so 2> /dev/null) 2> /dev/null)])
            ])

# Note that CUDA support is off by default.  To turn it on, the user has to
# request it.  The user can just ask for --with-cuda and it that case we
# look for the cuda.h file in /usr/local/cuda.  Otherwise, they can give
# us a directory.  If they provide a directory, we will look in that directory
# as well as the directory with the "include" string appended to it.  The fact
# that we check in two directories precludes us from using the OMPI_CHECK_DIR
# macro as that would error out after not finding it in the first directory.
# Note that anywhere CUDA aware code is in the Open MPI repository requires
# us to make use of AC_REQUIRE to ensure this check has been done.
AS_IF([test "$with_cuda" = "no" || test "x$with_cuda" = "x"],
      [opal_check_cuda_happy="no"
       AC_MSG_RESULT([not set (--with-cuda=$with_cuda)])],
      [AS_IF([test "$with_cuda" = "yes"],
             [AS_IF([test "x`ls /usr/local/cuda/include/cuda.h 2> /dev/null`" = "x"],
                    [AC_MSG_RESULT([not found in standard location])
                     AC_MSG_WARN([Expected file /usr/local/cuda/include/cuda.h not found])
                     AC_MSG_ERROR([Cannot continue])],
                    [AC_MSG_RESULT([found])
                     opal_check_cuda_happy=yes
                     opal_cuda_incdir=/usr/local/cuda/include])],
             [AS_IF([test ! -d "$with_cuda"],
                    [AC_MSG_RESULT([not found])
                     AC_MSG_WARN([Directory $with_cuda not found])
                     AC_MSG_ERROR([Cannot continue])],
                    [AS_IF([test "x`ls $with_cuda/include/cuda.h 2> /dev/null`" = "x"],
                           [AS_IF([test "x`ls $with_cuda/cuda.h 2> /dev/null`" = "x"],
                                  [AC_MSG_RESULT([not found])
                                   AC_MSG_WARN([Could not find cuda.h in $with_cuda/include or $with_cuda])
                                   AC_MSG_ERROR([Cannot continue])],
                                  [opal_check_cuda_happy=yes
                                   opal_cuda_incdir=$with_cuda
                                   AC_MSG_RESULT([found ($with_cuda/cuda.h)])])],
                           [opal_check_cuda_happy=yes
                            opal_cuda_incdir="$with_cuda/include"
                            AC_MSG_RESULT([found ($opal_cuda_incdir/cuda.h)])])])])])

AS_IF([test "$opal_check_cuda_happy" = "yes"],
    [OAC_CHECK_PACKAGE([cuda],
                       [$1],
                       [cuda.h],
                       [cuda],
                       [cuMemFree],
                       [opal_check_cuda_happy="yes"],
                       [opal_check_cuda_happy="no"])],
    [])

# We require CUDA IPC support which started in CUDA 4.1. Error
# out if the support is not there.
AS_IF([test "$opal_check_cuda_happy" = "yes"],
    [AC_CHECK_MEMBER([struct CUipcMemHandle_st.reserved],
        [],
        [AC_MSG_ERROR([Cannot continue because CUDA 4.1 or later is required])],
        [#include <$opal_cuda_incdir/cuda.h>])],
    [])

# If we have CUDA support, check to see if we have support for cuMemCreate memory on host NUMA.
AS_IF([test "$opal_check_cuda_happy"="yes"],
    [AC_CHECK_DECL([CU_MEM_LOCATION_TYPE_HOST_NUMA], [CUDA_VMM_SUPPORT=1], [CUDA_VMM_SUPPORT=0],
        [#include <$opal_cuda_incdir/cuda.h>])],
    [])

# If we have CUDA support, check to see if we have support for SYNC_MEMOPS
# which was first introduced in CUDA 6.0.
AS_IF([test "$opal_check_cuda_happy" = "yes"],
    [AC_CHECK_DECL([CU_POINTER_ATTRIBUTE_SYNC_MEMOPS], [CUDA_SYNC_MEMOPS=1], [CUDA_SYNC_MEMOPS=0],
        [#include <$opal_cuda_incdir/cuda.h>])],
    [])

# If we have CUDA support, check to see if we have CUDA 6.0 or later.
AC_COMPILE_IFELSE(
    [AC_LANG_PROGRAM([[#include <$opal_cuda_incdir/cuda.h>]],
        [[
#if CUDA_VERSION < 6000
#error "CUDA_VERSION is less than 6000"
#endif
        ]])],
        [CUDA_VERSION_60_OR_GREATER=1],
        [CUDA_VERSION_60_OR_GREATER=0])

# If we have CUDA support, check to see if we have support for cuPointerGetAttributes
# which was first introduced in CUDA 7.0.
AS_IF([test "$opal_check_cuda_happy" = "yes"],
    [AC_CHECK_DECL([cuPointerGetAttributes], [CUDA_GET_ATTRIBUTES=1], [CUDA_GET_ATTRIBUTES=0],
        [#include <$opal_cuda_incdir/cuda.h>])],
    [])

AC_MSG_CHECKING([if have cuda support])
if test "$opal_check_cuda_happy" = "yes"; then
    AC_MSG_RESULT([yes (-I$opal_cuda_incdir)])
    CUDA_SUPPORT=1
    common_cuda_CPPFLAGS="-I$opal_cuda_incdir"
    AC_SUBST([common_cuda_CPPFLAGS])
else
    AC_MSG_RESULT([no])
    CUDA_SUPPORT=0
fi

OPAL_SUMMARY_ADD([Accelerators], [CUDA support], [], [$opal_check_cuda_happy])

AM_CONDITIONAL([OPAL_cuda_support], [test "x$CUDA_SUPPORT" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_SUPPORT],$CUDA_SUPPORT,
                   [Whether we want cuda device pointer support])

AM_CONDITIONAL([OPAL_cuda_vmm_support], [test "x$CUDA_VMM_SUPPORT" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_VMM_SUPPORT],$CUDA_VMM_SUPPORT,
                   [Whether we have CU_MEM_LOCATION_TYPE_HOST_NUMA support available])

AM_CONDITIONAL([OPAL_cuda_sync_memops], [test "x$CUDA_SYNC_MEMOPS" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_SYNC_MEMOPS],$CUDA_SYNC_MEMOPS,
                   [Whether we have CUDA CU_POINTER_ATTRIBUTE_SYNC_MEMOPS support available])

AM_CONDITIONAL([OPAL_cuda_get_attributes], [test "x$CUDA_GET_ATTRIBUTES" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_GET_ATTRIBUTES],$CUDA_GET_ATTRIBUTES,
                   [Whether we have CUDA cuPointerGetAttributes function available])

# There is nothing specific we can check for to see if GPU Direct RDMA is available.
# Therefore, we check to see whether we have CUDA 6.0 or later.
AM_CONDITIONAL([OPAL_cuda_gdr_support], [test "x$CUDA_VERSION_60_OR_GREATER" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_GDR_SUPPORT],$CUDA_VERSION_60_OR_GREATER,
                   [Whether we have CUDA GDR support available])

CPPFLAGS=${cuda_save_CPPFLAGS}
LDFLAGS=${cuda_save_LDFLAGS}
LIBS=${cuda_save_LIBS}
OPAL_VAR_SCOPE_POP
])

