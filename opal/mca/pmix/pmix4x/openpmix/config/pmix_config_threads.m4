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
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_CONFIG_THREADS],[
#
# Arguments: none
#
# Dependencies: None
#
# Modifies:
#  none - see called tests
#
# configure threads
#

#
# Check we have POSIX threads
#
PMIX_CONFIG_POSIX_THREADS(HAVE_POSIX_THREADS=1, HAVE_POSIX_THREADS=0)
AC_MSG_CHECKING([for working POSIX threads package])
if test "$HAVE_POSIX_THREADS" = "1" ; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
export HAVE_POSIX_THREADS

#
# Ask what threading we want (allow posix right now)
#

if test "$HAVE_POSIX_THREADS" = "0"; then
    AC_MSG_WARN(["*** POSIX threads are not"])
    AC_MSG_WARN(["*** available on your system "])
    AC_MSG_ERROR(["*** Can not continue"])
fi

THREAD_CFLAGS="$PTHREAD_CFLAGS"
THREAD_CPPFLAGS="$PTHREAD_CPPFLAGS"
THREAD_LDFLAGS="$PTHREAD_LDFLAGS"
THREAD_LIBS="$PTHREAD_LIBS"

PMIX_CHECK_PTHREAD_PIDS

AC_DEFINE_UNQUOTED([PMIX_ENABLE_MULTI_THREADS], [1],
                   [Whether we should enable thread support within the PMIX code base])

])dnl
