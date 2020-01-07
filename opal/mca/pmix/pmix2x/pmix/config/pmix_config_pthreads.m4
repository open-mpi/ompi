dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2014-2016 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl PMIX_CONFIG_POSIX_THREADS()
dnl
dnl Configure posix threads, setting the following variables (but
dnl  not calling AC_SUBST on them).

# ********************************************************************
#
# Internal macros - do not call from outside PMIX_CONFIG_POSIX_THREADS
#
# ********************************************************************


AC_DEFUN([PMIX_INTL_PTHREAD_TRY_LINK], [
# BEGIN: PMIX_INTL_PTHREAD_TRY_LINK
#
# Make sure that we can run a small application in C or C++, which
# ever is the current language.  Do make sure that C or C++ is the
# current language.
#
# As long as this is not being run....
# pthread_t may be anything from an int to a struct -- init with self-tid.
#
    AC_LINK_IFELSE([AC_LANG_SOURCE([[
#include <pthread.h>

int i = 3;
pthread_t me, newthread;

void cleanup_routine(void *foo);
void *thread_main(void *foo);

void cleanup_routine(void *foo) { i = 4; }
void *thread_main(void *foo) { i = 2; return (void*) &i; }

int main(int argc, char* argv[])
{
    pthread_attr_t attr;

    me = pthread_self();
    pthread_atfork(NULL, NULL, NULL);
    pthread_attr_init(&attr);
    pthread_cleanup_push(cleanup_routine, 0);
    pthread_create(&newthread, &attr, thread_main, 0);
    pthread_join(newthread, 0);
    pthread_cleanup_pop(0);

    return 0;
}]])],
                 [$1], [$2])
# END: PMIX_INTL_PTHREAD_TRY_LINK
])dnl

# ********************************************************************
#
# Try to compile thread support without any special flags
#
# ********************************************************************
AC_DEFUN([PMIX_INTL_POSIX_THREADS_PLAIN_C], [
#
# C compiler
#
if test "$pmix_pthread_c_success" = "0"; then
  AC_MSG_CHECKING([if C compiler and POSIX threads work as is])

  AC_LANG_PUSH(C)
  PMIX_INTL_PTHREAD_TRY_LINK(pmix_pthread_c_success=1,
                            pmix_pthread_c_success=0)
  AC_LANG_POP(C)
  if test "$pmix_pthread_c_success" = "1"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi
fi
])dnl


AC_DEFUN([PMIX_INTL_POSIX_THREADS_PLAIN], [
# BEGIN: PMIX_INTL_POSIX_THREADS_PLAIN
#
# Check if can compile without any special flags
# we throw -D_REENTRANT or -D_THREAD_SAFE in here, just in
# case.  Some systems (OS X, for example) generally don't need
# the defines, but then will on one system header here or there
# why take chances?
#

AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [PMIX_INTL_POSIX_THREADS_PLAIN_C],
                  [pmix_pthread_c_success=1])

# End: PMIX_INTL_POSIX_THREADS_PLAIN
])dnl


# ********************************************************************
#
# Try to compile thread support with special compiler flags
#
# ********************************************************************
AC_DEFUN([PMIX_INTL_POSIX_THREADS_SPECIAL_FLAGS_C], [
#
# C compiler
#
if test "$pmix_pthread_c_success" = "0"; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if C compiler and POSIX threads work with $pf])
    CFLAGS="$orig_CFLAGS $pf"
    AC_LANG_PUSH(C)
    PMIX_INTL_PTHREAD_TRY_LINK(pmix_pthread_c_success=1,
                              pmix_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$pmix_pthread_c_success" = "1"; then
      PTHREAD_CFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      PTHREAD_CFLAGS=
      CFLAGS="$orig_CFLAGS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])


AC_DEFUN([PMIX_INTL_POSIX_THREADS_SPECIAL_FLAGS],[
# Begin: PMIX_INTL_POSIX_THREADS_SPECIAL_FLAGS
#
# If above didn't work, try some super-special compiler flags
# that get evaluated to the "right" things.
#
# -Kthread:
# -kthread:  FreeBSD kernel threads
# -pthread:  Modern GCC (most all platforms)
# -pthreads: GCC on solaris
# -mthreads:
# -mt:       Solaris native compilers / HP-UX aCC
#
# Put -mt before -mthreads because HP-UX aCC will properly compile
# with -mthreads (reading as -mt), but emit a warning about unknown
# flags hreads.  Stupid compilers.

case "${host_cpu}-${host_os}" in
  *solaris*)
    pflags="-pthread -pthreads -mt"
  ;;
  *)
    pflags="-Kthread -kthread -pthread -pthreads -mt -mthreads"
  ;;
esac

AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [PMIX_INTL_POSIX_THREADS_SPECIAL_FLAGS_C],
                  [pmix_pthread_c_success=1])

# End: PMIX_INTL_POSIX_THREADS_SPECIAL_FLAGS
])dnl


# ********************************************************************
#
# Try to compile thread support with extra libs
#
# ********************************************************************
AC_DEFUN([PMIX_INTL_POSIX_THREADS_LIBS_C],[
#
# C compiler
#
if test "$pmix_pthread_c_success" = "0"; then
  for pl in $plibs; do
    AC_MSG_CHECKING([if C compiler and POSIX threads work with $pl])
    case "${host_cpu}-${host-_os}" in
      *-aix* | *-freebsd*)
        if test "`echo $CPPFLAGS | $GREP 'D_THREAD_SAFE'`" = ""; then
          PTHREAD_CPPFLAGS="-D_THREAD_SAFE"
          CPPFLAGS="$CPPFLAGS $PTHREAD_CPPFLAGS"
        fi
      ;;
      *)
        if test "`echo $CPPFLAGS | $GREP 'D_REENTRANT'`" = ""; then
          PTHREAD_CPPFLAGS="-D_REENTRANT"
          CPPFLAGS="$CPPFLAGS $PTHREAD_CPPFLAGS"
        fi
      ;;
    esac
    LIBS="$orig_LIBS $pl"
    AC_LANG_PUSH(C)
    PMIX_INTL_PTHREAD_TRY_LINK(pmix_pthread_c_success=1,
                              pmix_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$pmix_pthread_c_success" = "1"; then
      PTHREAD_LIBS="$pl"
      AC_MSG_RESULT([yes])
    else
      PTHREAD_CPPFLAGS=
      CPPFLAGS="$orig_CPPFLAGS"
      LIBS="$orig_LIBS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])dnl

AC_DEFUN([PMIX_INTL_POSIX_THREADS_LIBS],[
# Begin: PMIX_INTL_POSIX_THREADS_LIBS
#
# if we can't find a super-special compiler flags, try some libraries.
# we throw -D_REENTRANT or -D_THREAD_SAFE in here, just in case.  Some
# systems (OS X, for example) generally don't need the defines, but
# then will on one system header here or there why take chances?
#
# libpthreads: AIX - must check before libpthread
# liblthread:  LinuxThreads on FreeBSD
# libpthread:  The usual place (like we can define usual!)
plibs="-lpthreads -llthread -lpthread"

AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [PMIX_INTL_POSIX_THREADS_LIBS_C],
                  [pmix_pthread_c_success=1])

# End: PMIX_INTL_POSIX_THREADS_LIBS]
)dnl


#********************************************************************
#
# External macro (aka, the real thing)
#
#********************************************************************
AC_DEFUN([PMIX_CONFIG_POSIX_THREADS],[
    AC_REQUIRE([AC_PROG_GREP])

pmix_pthread_c_success=0

orig_CFLAGS="$CFLAGS"
orig_CPPFLAGS="$CPPFLAGS"
orig_LDFLAGS="$LDFLAGS"
orig_LIBS="$LIBS"

PTHREAD_CFLAGS=
PTHREAD_CPPFLAGS=
PTHREAD_LDFLAGS=
PTHREAD_LIBS=

# Try with the basics, mam.
PMIX_INTL_POSIX_THREADS_PLAIN

# Try the super-special compiler flags.
PMIX_INTL_POSIX_THREADS_SPECIAL_FLAGS

# Try the normal linking methods (that's no fun)
PMIX_INTL_POSIX_THREADS_LIBS

#
# check to see if we can set error checking mutexes
#

# LinuxThreads
AC_MSG_CHECKING([for PTHREAD_MUTEX_ERRORCHECK_NP])
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
        [[#include <pthread.h>]],
        [[pthread_mutexattr_settype(NULL, PTHREAD_MUTEX_ERRORCHECK_NP);]])],
    [result="yes" defval=1], [result="no" defval=0])
AC_MSG_RESULT([$result])
AC_DEFINE_UNQUOTED([PMIX_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP], [$defval],
            [If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK_NP])

# Mac OS X
AC_MSG_CHECKING([for PTHREAD_MUTEX_ERRORCHECK])
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
        [[#include <pthread.h>]],
        [[pthread_mutexattr_settype(NULL, PTHREAD_MUTEX_ERRORCHECK);]])],
    [result="yes" defval=1], [result="no" defval=0])
AC_MSG_RESULT([$result])
AC_DEFINE_UNQUOTED([PMIX_HAVE_PTHREAD_MUTEX_ERRORCHECK], [$defval],
            [If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK])

CFLAGS="$orig_CFLAGS"
CPPFLAGS="$orig_CPPFLAGS"
LDFLAGS="$orig_LDFLAGS"
LIBS="$orig_LIBS"

if test "$pmix_pthread_c_success" = "1"; then
  internal_useless=1
  $1
else
  internal_useless=1
  $2
fi

unset pmix_pthread_c_success
unset internal_useless
])dnl
