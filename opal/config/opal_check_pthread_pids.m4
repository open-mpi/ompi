dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_PTHREAD_PIDS],[
#
# Arguments: none
#
# Dependencies: None
#
# Sets:
#  OPAL_THREADS_HAVE_DIFFERENT_PIDS (variable)
#
# Test for Linux-like threads in the system.  OPAL no longer supports
# systems with different PIDs for threads in the same process, so error
# out if we detect that case.
#

AC_MSG_CHECKING([if threads have different pids (pthreads on linux)])
CFLAGS_save="$CFLAGS"
CFLAGS="$CFLAGS $THREAD_CFLAGS"
CPPFLAGS_save="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
LDFLAGS_save="$LDFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
LIBS_save="$LIBS"
LIBS="$LIBS $THREAD_LIBS"
AC_TRY_RUN([#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
void *checkpid(void *arg);
int main() {
  pthread_t thr;
  int pid, *retval;
  pid = getpid();
  pthread_create(&thr, NULL, checkpid, &pid);
  pthread_join(thr, (void **) &retval);
  exit(*retval);
}
static int ret;
void *checkpid(void *arg) {
   int ppid = *((int *) arg);
   if (ppid == getpid())
     ret = 0;
   else
     ret = 1;
   pthread_exit((void *) &ret);
}], 
[MSG=no OPAL_THREADS_HAVE_DIFFERENT_PIDS=0], 
[MSG=yes OPAL_THREADS_HAVE_DIFFERENT_PIDS=1],
[case $host in
     *-linux*)

         # We don't know exactly when (and I really don't feel like
         # checking, because this is such a small case), but somewhere
         # along the line, Linux stopped using different PIDs for
         # threads.  So rule that if we're <=2.6.9 (i.e., RHEL4), then
         # the PIDs are different.  If above that version, they're the
         # same.
         linux_ver=`uname -a | awk '{ print [$]3 }'`
         linux_ver_major=`echo $linux_ver | cut -d. -f1`
         linux_ver_minor=`echo $linux_ver | cut -d. -f2`
         linux_ver_rel=`echo $linux_ver | cut -d. -f3 | cut -d- -f1`

         OPAL_THREADS_HAVE_DIFFERENT_PIDS=1
         MSG="cross compiling - assuming yes"

         if test "$linux_ver_major" -gt 2; then
             OPAL_THREADS_HAVE_DIFFERENT_PIDS=0
             MSG="cross compiling and Linux version >= 2.x.x - assuming no"
         elif test "$linux_ver_major" -eq 2 -a "$linux_ver_minor" -gt 6; then
             MSG="cross compiling and Linux version >= 2.6.x - assuming no"
         elif test "$linux_ver_major" -eq 2 -a "$linux_ver_minor" -eq 6 -a "$linux_ver_rel" -gt 9; then
             MSG="cross compiling and Linux version >= 2.6.9 - assuming no"
         fi
         ;;
     *)
         MSG="cross compiling - assuming no"
         OPAL_THREADS_HAVE_DIFFERENT_PIDS=0
         ;;
 esac
])

CFLAGS="$CFLAGS_save"
CPPFLAGS="$CPPFLAGS_save"
LDFLAGS="$LDFLAGS_save"
LIBS="$LIBS_save"

AC_MSG_RESULT([$MSG])
AS_IF([test "$OPAL_THREADS_HAVE_DIFFERENT_PIDS" = "1"],
      [AC_MSG_WARN([This version of Open MPI only supports when threads have])
       AC_MSG_WARN([the same PID.  Please use an older version of Open MPI])
       AC_MSG_WARN([if you need support on systems with different PIDs for])
       AC_MSG_WARN([threads in the same process.  Open MPI 1.4.x supports such])
       AC_MSG_WARN([ systems, as does at least some versions the Open MPI])
       AC_MSG_WARN([1.5.x series.])
       AC_MSG_ERROR([Cannot continue])
      ])

#
# if pthreads is not available, then the system does not have an insane threads
# model
#
unset MSG])dnl
