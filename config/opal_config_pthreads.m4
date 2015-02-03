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
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2014-2015 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl OPAL_CONFIG_POSIX_THREADS()
dnl
dnl Configure posix threads, setting the following variables (but
dnl  not calling AC_SUBST on them).

# ********************************************************************
#
# Internal macros - do not call from outside OPAL_CONFIG_POSIX_THREADS
#
# ********************************************************************


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
    pthread_join(&newthread, 0);
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

unset HAPPY opal_conftest_h
rm -rf conftest*
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
if test "$opal_pthread_fortran_success" = "0" && test "$OMPI_WANT_FORTRAN_BINDINGS" = "1" && test $ompi_fortran_happy -eq 1; then
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
      PTHREAD_CXXFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      PTHREAD_CXXFLAGS=
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
if test "$opal_pthread_fortran_success" = "0" && test "$OMPI_WANT_FORTRAN_BINDINGS" = "1" && test $ompi_fortran_happy -eq 1; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if Fortran compiler and POSIX threads work with $pf])
    FCFLAGS="$orig_FCFLAGS $pf"
    AC_LANG_PUSH(C)
    OPAL_INTL_PTHREAD_TRY_LINK_FORTRAN(opal_pthread_fortran_success=1, 
                                       opal_pthread_fortran_success=0)
    AC_LANG_POP(C)
    if test "$opal_pthread_fortran_success" = "1"; then
      PTHREAD_FCFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      PTHREAD_FCFLAGS=
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
    OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_c_success=1,
                              opal_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$opal_pthread_c_success" = "1"; then
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


AC_DEFUN([OPAL_INTL_POSIX_THREADS_LIBS_CXX],[
#
# C++ compiler
#
if test "$opal_pthread_cxx_success" = "0"; then
  if test ! "$opal_pthread_c_success" = "0" && test ! "$PTHREAD_LIBS" = "" ; then
    AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $PTHREAD_LIBS])
    case "${host_cpu}-${host-_os}" in
      *-aix* | *-freebsd*)
        if test "`echo $CXXCPPFLAGS | $GREP 'D_THREAD_SAFE'`" = ""; then
          PTHREAD_CXXCPPFLAGS="-D_THREAD_SAFE"
          CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
        fi
      ;;
      *)
        if test "`echo $CXXCPPFLAGS | $GREP 'D_REENTRANT'`" = ""; then
          PTHREAD_CXXCPPFLAGS="-D_REENTRANT"
          CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
        fi
      ;;
    esac
    LIBS="$orig_LIBS $PTHREAD_LIBS"
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
            PTHREAD_CXXCPPFLAGS="-D_THREAD_SAFE"
            CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
          fi
        ;;
        *)
          if test "`echo $CXXCPPFLAGS | $GREP 'D_REENTRANT'`" = ""; then
            PTHREAD_CXXCPPFLAGS="-D_REENTRANT"
            CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
          fi
        ;;
      esac
      LIBS="$orig_LIBS $pl"
      AC_LANG_PUSH(C++)
      OPAL_INTL_PTHREAD_TRY_LINK(opal_pthread_cxx_success=1, 
                                opal_pthread_cxx_success=0)
      AC_LANG_POP(C++)
      if test "$opal_pthread_cxx_success" = "1"; then
	PTHREAD_LIBS="$pl"
        AC_MSG_RESULT([yes])
      else
        PTHREAD_CXXCPPFLAGS=
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
if test "$opal_pthread_fortran_success" = "0" && test "$OMPI_WANT_FORTRAN_BINDINGS" = "1" && test $ompi_fortran_happy -eq 1; then
  if test ! "$opal_pthread_c_success" = "0" && test ! "$PTHREAD_LIBS" = "" ; then
    AC_MSG_CHECKING([if Fortran compiler and POSIX threads work with $PTHREAD_LIBS])
    LIBS="$orig_LIBS $PTHREAD_LIBS"
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
	PTHREAD_LIBS="$pl"
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

# End: OPAL_INTL_POSIX_THREADS_LIBS]
)dnl


#********************************************************************
#
# External macro (aka, the real thing)
#
#********************************************************************
AC_DEFUN([OPAL_CONFIG_POSIX_THREADS],[
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

PTHREAD_CFLAGS=
PTHREAD_FCFLAGS=
PTHREAD_CXXFLAGS=
PTHREAD_CPPFLAGS=
PTHREAD_CXXCPPFLAGS=
PTHREAD_LDFLAGS=
PTHREAD_LIBS=

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
    [result="yes" defval=1], [result="no" defval=0])
AC_MSG_RESULT([$result])
AC_DEFINE_UNQUOTED([OPAL_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP], [$defval],
            [If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK_NP])

# Mac OS X
AC_MSG_CHECKING([for PTHREAD_MUTEX_ERRORCHECK])
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
        [[#include <pthread.h>]],
        [[pthread_mutexattr_settype(NULL, PTHREAD_MUTEX_ERRORCHECK);]])],
    [result="yes" defval=1], [result="no" defval=0])
AC_MSG_RESULT([$result])
AC_DEFINE_UNQUOTED([OPAL_HAVE_PTHREAD_MUTEX_ERRORCHECK], [$defval],
            [If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK])

CFLAGS="$orig_CFLAGS"
FCFLAGS="$orig_FCFLAGS"
CXXFLAGS="$orig_CXXFLAGS"
CPPFLAGS="$orig_CPPFLAGS"
CXXCPPFLAGS="$orig_CXXCPPFLAGS"
LDFLAGS="$orig_LDFLAGS"
LIBS="$orig_LIBS"

if test "$OMPI_WANT_FORTRAN_BINDINGS" != "1" || test $ompi_fortran_happy -ne 1; then
  opal_pthread_fortran_success=1
fi

if test "$opal_pthread_c_success" = "1" && \
   test "$opal_pthread_cxx_success" = "1" && \
   test "$opal_pthread_fortran_success" = "1"; then
  internal_useless=1
  $1
else
  internal_useless=1
  $2
fi

unset opal_pthread_c_success opal_pthread_fortran_success opal_pthread_cxx_success
unset internal_useless
])dnl
