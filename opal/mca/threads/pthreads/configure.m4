# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2019      Triad National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# ********************************************************************
#
# Internal macros - do not call from outside OPAL_CONFIG_POSIX_THREADS
#
# ********************************************************************


AC_DEFUN([OPAL_CHECK_PTHREAD_PIDS],[
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

OPAL_VAR_SCOPE_PUSH([CFLAGS_save CPPFLAGS_save LDFLAGS_save LIBS_save MSG])
CFLAGS_save="$CFLAGS"
CFLAGS="$CFLAGS $THREAD_CFLAGS"
CPPFLAGS_save="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
LDFLAGS_save="$LDFLAGS"
LDFLAGS="$THREAD_LDFLAGS"
LIBS_save="$LIBS"
LIBS="$THREAD_LIBS"
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
[MSG=no OPAL_THREADS_HAVE_DIFFERENT_PIDS=0],
[MSG=yes OPAL_THREADS_HAVE_DIFFERENT_PIDS=1],
[
 # If we're cross compiling, we can't do another AC_* function here because
 # it we haven't displayed the result from the last one yet.  So defer
 # another test until below.
 OPAL_THREADS_HAVE_DIFFERENT_PIDS=
 MSG="cross compiling (need another test)"])

CFLAGS="$CFLAGS_save"
CPPFLAGS="$CPPFLAGS_save"
LDFLAGS="$LDFLAGS_save"
LIBS="$LIBS_save"

AC_MSG_RESULT([$MSG])

AS_IF([test "x$OPAL_THREADS_HAVE_DIFFERENT_PIDS" = "x"],
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
                     [OPAL_THREADS_HAVE_DIFFERENT_PIDS=1])])

AS_IF([test "$OPAL_THREADS_HAVE_DIFFERENT_PIDS" = "1"],
      [AC_MSG_WARN([This version of Open MPI only supports environments where])
       AC_MSG_WARN([threads have the same PID.  Please use an older version of])
       AC_MSG_WARN([Open MPI if you need support on systems with different])
       AC_MSG_WARN([PIDs for threads in the same process.  Open MPI 1.4.x])
       AC_MSG_WARN([supports such systems, as does at least some versions the])
       AC_MSG_WARN([Open MPI 1.5.x series.])
       AC_MSG_ERROR([Cannot continue])
      ])

#
# if pthreads is not available, then the system does not have an insane threads
# model
#
OPAL_VAR_SCOPE_POP
])dnl

AC_DEFUN([OPAL_INTL_PTHREAD_TRY_LINK], [
# BEGIN: OPAL_INTL_PTHREAD_TRY_LINK
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
# END: OPAL_INTL_PTHREAD_TRY_LINK
])dnl


AC_DEFUN([OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN], [
# BEGIN: OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN
#
# Make sure that we can run a small application in Fortran, with
# pthreads living in a C object file

OPAL_VAR_SCOPE_PUSH([HAPPY opal_conftest_h])

# Fortran module
cat > conftestf.f <<EOF
      program fpthread
      call pthreadtest
      end
EOF

# C module
if test -f conftest.h; then
    opal_conftest_h="#include \"conftest.h\""
else
    opal_conftest_h=""
fi
cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
$opal_conftest_h

#ifdef __cplusplus
extern "C" {
#endif
int i = 3;
pthread_t me, newthread;

void cleanup_routine(void *foo);
void *thread_main(void *foo);

void pthreadtest_f(void);
void pthreadtest(void);
void pthreadtest_(void);
void pthreadtest__(void);
void PTHREADTEST(void);

void cleanup_routine(void *foo) { i = 4; }
void *thread_main(void *foo) { i = 2; return (void*) &i; }

void pthreadtest_f(void)
{
    pthread_attr_t attr;

    me = pthread_self();
    pthread_atfork(NULL, NULL, NULL);
    pthread_attr_init(&attr);
    pthread_cleanup_push(cleanup_routine, 0);
    pthread_create(&newthread, &attr, thread_main, 0);
    pthread_join(newthread, 0);
    pthread_cleanup_pop(0);
}

void pthreadtest(void)
{ pthreadtest_f(); }

void pthreadtest_(void)
{ pthreadtest_f(); }

void pthreadtest__(void)
{ pthreadtest_f(); }

void PTHREADTEST(void)
{ pthreadtest_f(); }

#ifdef __cplusplus
}
#endif
EOF

# Try the compile
OPAL_LOG_COMMAND(
    [$CC $CFLAGS -I. -c conftest.c],
    OPAL_LOG_COMMAND(
        [$FC $FCFLAGS conftestf.f conftest.o -o conftest $LDFLAGS $LIBS],
        [HAPPY=1],
	[HAPPY=0]),
    [HAPPY=0])

if test "$HAPPY" = "1"; then
   $1
else
    OPAL_LOG_MSG([here is the C program:], 1)
    OPAL_LOG_FILE([conftest.c])
    if test -f conftest.h; then
	OPAL_LOG_MSG([here is contest.h:], 1)
	OPAL_LOG_FILE([conftest.h])
    fi
    OPAL_LOG_MSG([here is the fortran program:], 1)
    OPAL_LOG_FILE([conftestf.f])
    $2
fi

rm -rf conftest*

OPAL_VAR_SCOPE_POP
# END: OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN
])dnl


# ********************************************************************
#
# Try to compile thread support without any special flags
#
# ********************************************************************
AC_DEFUN([OPAL_INTL_POSIX_THREADS_PLAIN_C], [
#
# C compiler
#
if test "$opal_pthread_c_success" = "0"; then
  AC_MSG_CHECKING([if C compiler and POSIX threads work as is])

  AC_LANG_PUSH(C)
  OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_c_success=1,
                            opal_pthread_c_success=0)
  AC_LANG_POP(C)
  if test "$opal_pthread_c_success" = "1"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi
fi
])dnl


AC_DEFUN([OPAL_INTL_POSIX_THREADS_PLAIN_CXX], [
#
# C++ compiler
#
if test "$opal_pthread_cxx_success" = "0"; then
  AC_MSG_CHECKING([if C++ compiler and POSIX threads work as is])

  AC_LANG_PUSH(C++)
  OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_cxx_success=1,
                            opal_pthread_cxx_success=0)
  AC_LANG_POP(C++)
  if test "$opal_pthread_cxx_success" = "1"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi
fi
])dnl


AC_DEFUN([OPAL_INTL_POSIX_THREADS_PLAIN_FC], [
#
# Fortran compiler
#
if test "$opal_pthread_fortran_success" = "0" && \
   test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" && \
   test $ompi_fortran_happy -eq 1; then
  AC_MSG_CHECKING([if Fortran compiler and POSIX threads work as is])

  AC_LANG_PUSH(C)
  OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN(opal_pthread_fortran_success=1,
                                     opal_pthread_fortran_success=0)
  AC_LANG_POP(C)
  if test "$opal_pthread_fortran_success" = "1"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi
fi
])dnl


AC_DEFUN([OPAL_INTL_POSIX_THREADS_PLAIN], [
# BEGIN: OPAL_INTL_POSIX_THREADS_PLAIN
#
# Check if can compile without any special flags
# we throw -D_REENTRANT or -D_THREAD_SAFE in here, just in
# case.  Some systems (OS X, for example) generally don't need
# the defines, but then will on one system header here or there
# why take chances?
#

# Only run C++ and Fortran if those compilers already configured
AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [OPAL_INTL_POSIX_THREADS_PLAIN_C],
                  [opal_pthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX],
                  [OPAL_INTL_POSIX_THREADS_PLAIN_CXX],
                  [opal_pthread_cxx_success=1])

AC_PROVIDE_IFELSE([AC_PROG_FC],
                  [OPAL_INTL_POSIX_THREADS_PLAIN_FC],
                  [opal_pthread_fortran_success=1])

# End: OPAL_INTL_POSIX_THREADS_PLAIN
])dnl


# ********************************************************************
#
# Try to compile thread support with special compiler flags
#
# ********************************************************************
AC_DEFUN([OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS_C], [
#
# C compiler
#
if test "$opal_pthread_c_success" = "0"; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if C compiler and POSIX threads work with $pf])
    CFLAGS="$orig_CFLAGS $pf"
    AC_LANG_PUSH(C)
    OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_c_success=1,
                              opal_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$opal_pthread_c_success" = "1"; then
      TPKG_CFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      TPKG_CFLAGS=
      CFLAGS="$orig_CFLAGS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])


AC_DEFUN([OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS_CXX], [
#
# C++ compiler
#
if test "$opal_pthread_cxx_success" = "0"; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $pf])
    CXXFLAGS="$orig_CXXFLAGS $pf"
    AC_LANG_PUSH(C++)
    OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_cxx_success=1,
                              opal_pthread_cxx_success=0)
    AC_LANG_POP(C++)
    if test "$opal_pthread_cxx_success" = "1"; then
      TPKG_CXXFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      TPKG_CXXFLAGS=
      CXXFLAGS="$orig_CXXFLAGS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])


AC_DEFUN([OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS_FC], [
#
# Fortran compiler
#
if test "$opal_pthread_fortran_success" = "0" && \
   test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" && \
   test $ompi_fortran_happy -eq 1; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if Fortran compiler and POSIX threads work with $pf])
    FCFLAGS="$orig_FCFLAGS $pf"
    AC_LANG_PUSH(C)
    OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN(opal_pthread_fortran_success=1,
                                       opal_pthread_fortran_success=0)
    AC_LANG_POP(C)
    if test "$opal_pthread_fortran_success" = "1"; then
      TPKG_FCFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      TPKG_FCFLAGS=
      FCFLAGS="$orig_FCFLAGS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])


AC_DEFUN([OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS],[
# Begin: OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS
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
OPAL_VAR_SCOPE_PUSH([pflags])

case "${host_cpu}-${host_os}" in
  *solaris*)
    pflags="-pthread -pthreads -mt"
  ;;
  *)
    pflags="-Kthread -kthread -pthread -pthreads -mt -mthreads"
  ;;
esac

# Only run C++ and Fortran if those compilers already configured
AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS_C],
                  [opal_pthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX],
                  [OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS_CXX],
                  [opal_pthread_cxx_success=1])

AC_PROVIDE_IFELSE([AC_PROG_FC],
                  [OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS_FC],
                  [opal_pthread_fortran_success=1])

OPAL_VAR_SCOPE_POP
# End: OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS
])dnl


# ********************************************************************
#
# Try to compile thread support with extra libs
#
# ********************************************************************
AC_DEFUN([OPAL_INTL_POSIX_THREADS_LIBS_C],[
#
# C compiler
#
if test "$opal_pthread_c_success" = "0"; then
  for pl in $plibs; do
    AC_MSG_CHECKING([if C compiler and POSIX threads work with $pl])
    case "${host_cpu}-${host-_os}" in
      *-aix* | *-freebsd*)
        if test "`echo $CPPFLAGS | $GREP 'D_THREAD_SAFE'`" = ""; then
          TPKG_CPPFLAGS="-D_THREAD_SAFE"
          CPPFLAGS="$CPPFLAGS $TPKG_CPPFLAGS"
        fi
      ;;
      *)
        if test "`echo $CPPFLAGS | $GREP 'D_REENTRANT'`" = ""; then
          TPKG_CPPFLAGS="-D_REENTRANT"
          CPPFLAGS="$CPPFLAGS $TPKG_CPPFLAGS"
        fi
      ;;
    esac
    LIBS="$orig_LIBS $pl"
    AC_LANG_PUSH(C)
    OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_c_success=1,
                              opal_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$opal_pthread_c_success" = "1"; then
      TPKG_LIBS="$pl"
      AC_MSG_RESULT([yes])
    else
      TPKG_CPPFLAGS=
      CPPFLAGS="$orig_CPPFLAGS"
      LIBS="$orig_LIBS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])dnl


AC_DEFUN([OPAL_INTL_POSIX_THREADS_LIBS_CXX],[
#
# C++ compiler
#
if test "$opal_pthread_cxx_success" = "0"; then
  if test ! "$opal_pthread_c_success" = "0" && test ! "$TPKG_LIBS" = "" ; then
    AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $TPKG_LIBS])
    case "${host_cpu}-${host-_os}" in
      *-aix* | *-freebsd*)
        if test "`echo $CXXCPPFLAGS | $GREP 'D_THREAD_SAFE'`" = ""; then
          TPKG_CXXCPPFLAGS="-D_THREAD_SAFE"
          CXXCPPFLAGS="$CXXCPPFLAGS $TPKG_CXXCPPFLAGS"
        fi
      ;;
      *)
        if test "`echo $CXXCPPFLAGS | $GREP 'D_REENTRANT'`" = ""; then
          TPKG_CXXCPPFLAGS="-D_REENTRANT"
          CXXCPPFLAGS="$CXXCPPFLAGS $TPKG_CXXCPPFLAGS"
        fi
      ;;
    esac
    LIBS="$orig_LIBS $TPKG_LIBS"
    AC_LANG_PUSH(C++)
    OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_cxx_success=1,
                              opal_pthread_cxx_success=0)
    AC_LANG_POP(C++)
    if test "$opal_pthread_cxx_success" = "1"; then
      AC_MSG_RESULT([yes])
    else
      CXXCPPFLAGS="$orig_CXXCPPFLAGS"
      LIBS="$orig_LIBS"
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Can not find working threads configuration.  aborting])
    fi
  else
    for pl in $plibs; do
      AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $pl])
      case "${host_cpu}-${host-_os}" in
        *-aix* | *-freebsd*)
          if test "`echo $CXXCPPFLAGS | $GREP 'D_THREAD_SAFE'`" = ""; then
            TPKG_CXXCPPFLAGS="-D_THREAD_SAFE"
            CXXCPPFLAGS="$CXXCPPFLAGS $TPKG_CXXCPPFLAGS"
          fi
        ;;
        *)
          if test "`echo $CXXCPPFLAGS | $GREP 'D_REENTRANT'`" = ""; then
            TPKG_CXXCPPFLAGS="-D_REENTRANT"
            CXXCPPFLAGS="$CXXCPPFLAGS $TPKG_CXXCPPFLAGS"
          fi
        ;;
      esac
      LIBS="$orig_LIBS $pl"
      AC_LANG_PUSH(C++)
      OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_cxx_success=1,
                                opal_pthread_cxx_success=0)
      AC_LANG_POP(C++)
      if test "$opal_pthread_cxx_success" = "1"; then
	TPKG_LIBS="$pl"
        AC_MSG_RESULT([yes])
      else
        TPKG_CXXCPPFLAGS=
        CXXCPPFLAGS="$orig_CXXCPPFLAGS"
        LIBS="$orig_LIBS"
        AC_MSG_RESULT([no])
      fi
    done
  fi
fi
])dnl


AC_DEFUN([OPAL_INTL_POSIX_THREADS_LIBS_FC],[
#
# Fortran compiler
#
if test $opal_pthread_fortran_success -eq 0 && \
   test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" && \
   test $ompi_fortran_happy -eq 1; then
  if test $opal_pthread_c_success -ne 0 && test ! "$TPKG_LIBS" = "" ; then
    AC_MSG_CHECKING([if Fortran compiler and POSIX threads work with $TPKG_LIBS])
    LIBS="$orig_LIBS $TPKG_LIBS"
    AC_LANG_PUSH(C)
    OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN(opal_pthread_fortran_success=1,
                                       opal_pthread_fortran_success=0)
    AC_LANG_POP(C)
    if test "$opal_pthread_fortran_success" = "1"; then
      AC_MSG_RESULT([yes])
    else
      LIBS="$orig_LIBS"
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Can not find working threads configuration.  aborting])
    fi
  else
    for pl in $plibs; do
      AC_MSG_CHECKING([if Fortran compiler and POSIX threads work with $pl])
      LIBS="$orig_LIBS $pl"
      AC_LANG_PUSH(C)
      OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN(opal_pthread_fortran_success=1,
                                         opal_pthread_fortran_success=0)
      AC_LANG_POP(C)
      if test "$opal_pthread_fortran_success" = "1"; then
	TPKG_LIBS="$pl"
        AC_MSG_RESULT([yes])
        break
      else
        LIBS="$orig_LIBS"
        AC_MSG_RESULT([no])
      fi
    done
  fi
fi
])dnl


AC_DEFUN([OPAL_INTL_POSIX_THREADS_LIBS],[
# Begin: OPAL_INTL_POSIX_THREADS_LIBS
#
# if we can't find a super-special compiler flags, try some libraries.
# we throw -D_REENTRANT or -D_THREAD_SAFE in here, just in case.  Some
# systems (OS X, for example) generally don't need the defines, but
# then will on one system header here or there why take chances?
#
# libpthreads: AIX - must check before libpthread
# liblthread:  LinuxThreads on FreeBSD
# libpthread:  The usual place (like we can define usual!)
OPAL_VAR_SCOPE_PUSH([plibs])
plibs="-lpthreads -llthread -lpthread"

# Only run C++ and Fortran if those compilers already configured
AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [OPAL_INTL_POSIX_THREADS_LIBS_C],
                  [opal_pthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX],
                  [OPAL_INTL_POSIX_THREADS_LIBS_CXX],
                  [opal_pthread_cxx_success=1])

AC_PROVIDE_IFELSE([AC_PROG_FC],
                  [OPAL_INTL_POSIX_THREADS_LIBS_FC],
                  [opal_pthread_fortran_success=1])

OPAL_VAR_SCOPE_POP
# End: OPAL_INTL_POSIX_THREADS_LIBS]
)dnl


#********************************************************************
#
# External macro (aka, the real thing)
#
#********************************************************************
AC_DEFUN([OPAL_CONFIG_POSIX_THREADS],[
    OPAL_VAR_SCOPE_PUSH([opal_pthreads_result defval opal_pthread_c_success opal_pthread_fortran_success opal_pthread_cxx_success orig_CFLAGS orig_FCFLAGS orig_CXXFLAGS orig_CPPFLAGS orig_CXXCPPFLAGS orig_LDFLAGS orig_LIBS])
    AC_REQUIRE([AC_PROG_GREP])

opal_pthread_c_success=0
opal_pthread_fortran_success=0
opal_pthread_cxx_success=0

orig_CFLAGS="$CFLAGS"
orig_FCFLAGS="$FCFLAGS"
orig_CXXFLAGS="$CXXFLAGS"
orig_CPPFLAGS="$CPPFLAGS"
orig_CXXCPPFLAGS="$CXXCPPFLAGS"
orig_LDFLAGS="$LDFLAGS"
orig_LIBS="$LIBS"

TPKG_CFLAGS=
TPKG_FCFLAGS=
TPKG_CXXFLAGS=
TPKG_CPPFLAGS=
TPKG_CXXCPPFLAGS=
TPKG_LDFLAGS=
TPKG_LIBS=

# Try with the basics, mam.
OPAL_INTL_POSIX_THREADS_PLAIN

# Try the super-special compiler flags.
OPAL_INTL_POSIX_THREADS_SPECIAL_FLAGS

# Try the normal linking methods (that's no fun)
OPAL_INTL_POSIX_THREADS_LIBS

#
# check to see if we can create shared memory mutexes and conditions
#
AC_CHECK_FUNCS([pthread_mutexattr_setpshared pthread_condattr_setpshared])

#
# check to see if we can set error checking mutexes
#

# LinuxThreads
AC_MSG_CHECKING([for PTHREAD_MUTEX_ERRORCHECK_NP])
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
        [[#include <pthread.h>]],
        [[pthread_mutexattr_settype(NULL, PTHREAD_MUTEX_ERRORCHECK_NP);]])],
    [opal_pthreads_result="yes" defval=1], [opal_pthreads_result="no" defval=0])
AC_MSG_RESULT([$opal_pthreads_result])
AC_DEFINE_UNQUOTED([OPAL_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP], [$defval],
            [If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK_NP])

# Mac OS X
AC_MSG_CHECKING([for PTHREAD_MUTEX_ERRORCHECK])
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
        [[#include <pthread.h>]],
        [[pthread_mutexattr_settype(NULL, PTHREAD_MUTEX_ERRORCHECK);]])],
    [opal_pthreads_result="yes" defval=1], [opal_pthreads_result="no" defval=0])
AC_MSG_RESULT([$opal_pthreads_result])
AC_DEFINE_UNQUOTED([OPAL_HAVE_PTHREAD_MUTEX_ERRORCHECK], [$defval],
            [If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK])

CFLAGS="$orig_CFLAGS"
FCFLAGS="$orig_FCFLAGS"
CXXFLAGS="$orig_CXXFLAGS"
CPPFLAGS="$orig_CPPFLAGS"
CXXCPPFLAGS="$orig_CXXCPPFLAGS"
LDFLAGS="$orig_LDFLAGS"
LIBS="$orig_LIBS"

THREAD_CFLAGS="$TPKG_CFLAGS"
THREAD_FCFLAGS="$TPKG_FCFLAGS"
THREAD_CXXFLAGS="$TPKG_CXXFLAGS"
THREAD_CPPFLAGS="$TPKG_CPPFLAGS"
THREAD_CXXCPPFLAGS="$TPKG_CXXCPPFLAGS"
THREAD_LDFLAGS="$TPKG_LDFLAGS"
THREAD_LIBS="$TPKG_LIBS"

OPAL_CHECK_PTHREAD_PIDS

if test "$OMPI_TRY_FORTRAN_BINDINGS" = "$OMPI_FORTRAN_NO_BINDINGS" || \
   test $ompi_fortran_happy -ne 1; then
    opal_pthread_fortran_success=1
fi

AC_MSG_CHECKING([if POSIX threads work])
if test $opal_pthread_c_success -eq 1 && \
   test $opal_pthread_cxx_success -eq 1 && \
   test $opal_pthread_fortran_success -eq 1; then
    AC_MSG_RESULT([yes])
    $1
else
    AC_MSG_RESULT([no])
    $2
fi

OPAL_VAR_SCOPE_POP
])dnl

AC_DEFUN([MCA_opal_threads_pthreads_PRIORITY], [30])

AC_DEFUN([MCA_opal_threads_pthreads_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# If component was selected, $1 will be 1 and we should set the base header
AC_DEFUN([MCA_opal_threads_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], 
          [opal_thread_type_found="pthreads"
           AC_DEFINE_UNQUOTED([MCA_threads_base_include_HEADER],
                              ["opal/mca/threads/pthreads/threads_pthreads_threads.h"],
                              [Header to include for threads implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_mutex_base_include_HEADER],
                              ["opal/mca/threads/pthreads/threads_pthreads_mutex.h"],
                              [Header to include for mutex implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_tsd_base_include_HEADER],
                              ["opal/mca/threads/pthreads/threads_pthreads_tsd.h"],
                              [Header to include for tsd implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_wait_sync_base_include_HEADER],
                              ["opal/mca/threads/pthreads/threads_pthreads_wait_sync.h"],
                              [Header to include for wait_sync implementation])
           THREAD_CFLAGS="$TPKG_CFLAGS"
           THREAD_FCFLAGS="$TPKG_FCFLAGS"
           THREAD_CXXFLAGS="$TPKG_CXXFLAGS"
           THREAD_CPPFLAGS="$TPKG_CPPFLAGS"
           THREAD_CXXCPPFLAGS="$TPKG_CXXCPPFLAGS"
           THREAD_LDFLAGS="$TPKG_LDFLAGS"
           THREAD_LIBS="$TPKG_LIBS"
           AC_SUBST(THREAD_CFLAGS)
           AC_SUBST(THREAD_FCFLAGS)
           AC_SUBST(THREAD_CXXFLAGS)
           AC_SUBST(THREAD_CPPFLAGS)
           AC_SUBST(THREAD_LDFLAGS)
           AC_SUBST(THREAD_LIBS)
          ])
])dnl

# MCA_threads_pthreads_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_threads_pthreads_CONFIG],[
    AC_CONFIG_FILES([opal/mca/threads/pthreads/Makefile])
    OPAL_VAR_SCOPE_PUSH([posix_thread_works])

    AS_IF([test -z "$with_threads" || test "$with_threads" = "pthreads" || test "$with_threads" = "yes"],
          [OPAL_CONFIG_POSIX_THREADS([posix_threads_works=1],[posix_threads_works=0])],
          [posix_threads_works=0])

    AS_IF([test $posix_threads_works -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP
])

