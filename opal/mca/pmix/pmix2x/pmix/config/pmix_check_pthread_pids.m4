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
dnl Copyright (c) 2008-2013 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2017      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_CHECK_PTHREAD_PIDS],[
#
# Arguments: none
#
# Dependencies: None
#
# Sets:
#  PMIX_THREADS_HAVE_DIFFERENT_PIDS (variable)
#
# Test for Linux-like threads in the system.  PMIX does not support
# systems with different PIDs for threads in the same process, so error
# out if we detect that case.
#

AC_MSG_CHECKING([if threads have different pids (pthreads on linux)])

PMIX_VAR_SCOPE_PUSH([tpids_CFLAGS_save tpids_CPPFLAGS_save tpids_LDFLAGS_save tpids_LIBS_save tpids_MSG])
tpids_CFLAGS_save="$CFLAGS"
CFLAGS="$CFLAGS $THREAD_CFLAGS"
tpids_CPPFLAGS_save="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
tpids_LDFLAGS_save="$LDFLAGS"
LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
tpids_LIBS_save="$LIBS"
LIBS="$LIBS $THREAD_LIBS"
AC_RUN_IFELSE([AC_LANG_SOURCE([#include <pthread.h>
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
}])],
[tpids_MSG=no PMIX_THREADS_HAVE_DIFFERENT_PIDS=0],
[tpids_MSG=yes PMIX_THREADS_HAVE_DIFFERENT_PIDS=1],
[
 # If we're cross compiling, we can't do another AC_* function here beause
 # it we haven't displayed the result from the last one yet.  So defer
 # another test until below.
 PMIX_THREADS_HAVE_DIFFERENT_PIDS=
 MSG="cross compiling (need another test)"])

CFLAGS="$tpids_CFLAGS_save"
CPPFLAGS="$tpids_CPPFLAGS_save"
LDFLAGS="$tpids_LDFLAGS_save"
LIBS="$tpids_LIBS_save"

AC_MSG_RESULT([$tpids_MSG])

AS_IF([test "x$PMIX_THREADS_HAVE_DIFFERENT_PIDS" = "x"],
      [ # If we are cross-compiling, look for the symbol
       # __linuxthreads_create_event, which seems to only exist in the
       # Linux Threads-based pthreads implementation (i.e., the one
       # that has different PIDs for each thread).  We *could* switch
       # on $host here and only test *linux* hosts, but this test is
       # pretty unique, so why bother?  Note that AC_CHECK_FUNC works
       # properly in cross-compiling environments in recent-enough
       # versions of Autoconf (which is one of the reasons we mandate
       # recent versions in autogen!).
       AC_CHECK_FUNC([__linuxthreads_create_event],
                     [PMIX_THREADS_HAVE_DIFFERENT_PIDS=1])])

AS_IF([test "$PMIX_THREADS_HAVE_DIFFERENT_PIDS" = "1"],
      [AC_MSG_WARN([This version of PMIx only supports environments where])
       AC_MSG_WARN([threads have the same PID])
       AC_MSG_ERROR([Cannot continue])
      ])

#
# if pthreads is not available, then the system does not have an insane threads
# model
#
PMIX_VAR_SCOPE_POP])dnl
