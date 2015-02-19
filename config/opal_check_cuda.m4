dnl -*- shell-script -*-
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
dnl Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2011-2014 NVIDIA Corporation.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CHECK_CUDA],[
#
# Check to see if user wants CUDA support
#
AC_ARG_WITH([cuda],
            [AC_HELP_STRING([--with-cuda(=DIR)],
            [Build cuda support, optionally adding DIR/include])])
AC_MSG_CHECKING([if --with-cuda is set])

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

dnl We cannot have CUDA support without dlopen support.  HOWEVER, at
dnl this point in configure, we can't know whether the DL framework
dnl has been configured or not yet (it likely hasn't, since CUDA is a
dnl common framework, and likely configured first).  So we have to
dnl defer this check until later (see the OPAL_CHECK_CUDA_AFTER_OPAL_DL m4
dnl macro, below).  :-(

# If we have CUDA support, check to see if we have CUDA 4.1 support
AS_IF([test "$opal_check_cuda_happy"="yes"],
    AC_CHECK_MEMBER([struct CUipcMemHandle_st.reserved], [CUDA_SUPPORT_41=1], [CUDA_SUPPORT_41=0],
        [#include <$opal_cuda_incdir/cuda.h>]),
    [])

# If we have CUDA support, check to see if we have support for SYNC_MEMOPS
# which was first introduced in CUDA 6.0.
AS_IF([test "$opal_check_cuda_happy"="yes"],
    AC_CHECK_DECL([CU_POINTER_ATTRIBUTE_SYNC_MEMOPS], [CUDA_SYNC_MEMOPS=1], [CUDA_SYNC_MEMOPS=0],
        [#include <$opal_cuda_incdir/cuda.h>]),
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
AS_IF([test "$opal_check_cuda_happy"="yes"],
    AC_CHECK_DECL([cuPointerGetAttributes], [CUDA_GET_ATTRIBUTES=1], [CUDA_GET_ATTRIBUTES=0],
        [#include <$opal_cuda_incdir/cuda.h>]),
    [])

AC_MSG_CHECKING([if have cuda support])
if test "$opal_check_cuda_happy" = "yes"; then
    AC_MSG_RESULT([yes (-I$opal_cuda_incdir)])
    CUDA_SUPPORT=1
    opal_datatype_cuda_CPPFLAGS="-I$opal_cuda_incdir"
    AC_SUBST([opal_datatype_cuda_CPPFLAGS])
else
    AC_MSG_RESULT([no])
    CUDA_SUPPORT=0
fi

AM_CONDITIONAL([OPAL_cuda_support], [test "x$CUDA_SUPPORT" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_SUPPORT],$CUDA_SUPPORT,
                   [Whether we want cuda device pointer support])

AM_CONDITIONAL([OPAL_cuda_support_41], [test "x$CUDA_SUPPORT_41" = "x1"])
AC_DEFINE_UNQUOTED([OPAL_CUDA_SUPPORT_41],$CUDA_SUPPORT_41,
                   [Whether we have CUDA 4.1 support available])

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

])

dnl
dnl CUDA support requires DL support (it dynamically opens the CUDA
dnl library at run time).  But we do not check for OPAL DL support
dnl until lafter the initial OPAL_CHECK_CUDA is called.  So put the
dnl CUDA+DL check in a separate macro that can be called after the DL MCA
dnl framework checks in the top-level configure.ac.
dnl
AC_DEFUN([OPAL_CHECK_CUDA_AFTER_OPAL_DL],[

    # We cannot have CUDA support without OPAL DL support.  Error out
    # if the user wants CUDA but we do not have OPAL DL support.
    AS_IF([test $OPAL_HAVE_DL_SUPPORT -eq 0 && \
           test "$opal_check_cuda_happy" = "yes"],
          [AC_MSG_WARN([--with-cuda was specified, but dlopen support is disabled.])
           AC_MSG_WARN([You must reconfigure Open MPI with dlopen ("dl") support.])
           AC_MSG_ERROR([Cannot continue.])])
])
