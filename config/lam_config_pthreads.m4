dnl
dnl $HEADER$
dnl
dnl LAM_CONFIG_POSIX_THREADS()
dnl
dnl Configure posix threads, setting the following variables (but
dnl  not calling AC_SUBST on them).

# ********************************************************************
#
# Internal macros - do not call from outside LAM_CONFIG_POSIX_THREADS
#
# ********************************************************************


AC_DEFUN([LAM_INTL_PTHREAD_TRY_LINK], [
# BEGIN: LAM_INTL_PTHREAD_TRY_LINK
#
# Make sure that we can run a small application in C or C++, which
# ever is the current language.  Do make sure that C or C++ is the
# current language.
    AC_TRY_LINK([#include <pthread.h>],
                 [pthread_t th; pthread_join(th, 0);
                 pthread_attr_init(0); pthread_cleanup_push(0, 0);
                 pthread_create(0,0,0,0); pthread_cleanup_pop(0); ],
                 [$1], [$2])
# END: LAM_INTL_PTHREAD_TRY_LINK
])dnl


AC_DEFUN([LAM_INTL_PTHREAD_TRY_LINK_F77], [
# BEGIN: LAM_INTL_PTHREAD_TRY_LINK_F77
#
# Make sure that we can run a small application in Fortran, with
# pthreads living in a C object file

# Fortran module
cat > conftestf.f <<EOF
      program fpthread
      INTEGER i
      i = 1
      end
EOF

# C module
if test -f conftest.h; then
    lam_conftest_h="#include \"conftest.h\""
else
    lam_conftest_h=""
fi
cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
$lam_conftest_h

#ifdef __cplusplus
extern "C" {
#endif
void lam_pthread()
{
  pthread_t th;
  pthread_join(th, 0);
  pthread_attr_init(0);
  pthread_cleanup_push(0, 0);
  pthread_create(0,0,0,0);
  pthread_cleanup_pop(0); 
}
#ifdef __cplusplus
}
#endif
EOF

# Try the compile
LAM_LOG_COMMAND(
    [$CC $CFLAGS -I. -c conftest.c],
    LAM_LOG_COMMAND(
        [$F77 $FFLAGS conftestf.f conftest.o -o conftest $LIBS],
        [HAPPY=1],
	[HAPPY=0]),
    [HAPPY=0])

if test "$HAPPY" = "1"; then
   $1
else
    LAM_LOG_MSG([here is the C program:], 1)
    LAM_LOG_FILE([conftest.c])
    if test -f conftest.h; then
	LAM_LOG_MSG([here is contest.h:], 1)
	LAM_LOG_FILE([conftest.h])
    fi
    LAM_LOG_MSG([here is the fortran program:], 1)
    LAM_LOG_FILE([conftestf.f])
    $2
fi

unset HAPPY lam_conftest_h
/bin/rm -f conftest*
# END: LAM_INTL_PTHREAD_TRY_LINK_F77
])dnl


# ********************************************************************
#
# Try to compile thread support without any special flags
#
# ********************************************************************
AC_DEFUN([LAM_INTL_POSIX_THREADS_PLAIN_C], [
#
# C compiler
#
if test "$lam_pthread_c_success" = "0"; then
  AC_MSG_CHECKING([if C compiler and POSIX threads work as is])
  if test "$HAVE_POSIX_THREADS" = "1" ; then
    run_this_test=1
  else
    case "${host_cpu}-${host_os}" in
      *solaris*)
        AC_MSG_RESULT([no - Solaris, not checked])
        run_this_test=0
      ;;
      *-aix* | *-freebsd*)
        if test "`echo $CPPFLAGS | grep 'D_THREAD_SAFE'`" = ""; then
          PTRHEAD_CPPFLAGS="-D_THREAD_SAFE"
          CPPFLAGS="$CPPFLAGS $PTHREAD_CPPFLAGS"
        fi
        run_this_test=1
      ;;
      *)
        if test "`echo $CPPFLAGS | grep 'D_REENTRANT'`" = ""; then
          PTHREAD_CPPFLAGS="-D_REENTRANT"
          CPPFLAGS="$CPPFLAGS $PTHREAD_CPPFLAGS"
        fi
        run_this_test=1
      ;;
    esac
  fi

  if test "$run_this_test" = "1" ; then
    AC_LANG_PUSH(C)
    LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_c_success=1,
                              lam_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$lam_pthread_c_success" = "1"; then
      AC_MSG_RESULT([yes])
    else
      PTHREAD_CPPFLAGS=
      CPPFLAGS="$orig_CPPFLAGS"
      AC_MSG_RESULT([no])
    fi
  fi
fi
])dnl


AC_DEFUN([LAM_INTL_POSIX_THREADS_PLAIN_CXX], [
#
# C++ compiler
#
if test "$lam_pthread_cxx_success" = "0"; then
  AC_MSG_CHECKING([if C++ compiler and POSIX threads work as is])
  if test "$HAVE_POSIX_THREADS" = "1" ; then
    run_this_test=1
  else
    case "${host_cpu}-${host_os}" in
      *solaris*)
        AC_MSG_RESULT([no - Solaris, not checked])
        run_this_test=0
      ;;
      *-aix* | *-freebsd*)
        if test "`echo $CXXCPPFLAGS | grep 'D_THREAD_SAFE'`" = ""; then
          PTRHEAD_CXXCPPFLAGS="-D_THREAD_SAFE"
          CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
        fi
        run_this_test=1
      ;;
      *)
        if test "`echo $CXXCPPFLAGS | grep 'D_REENTRANT'`" = ""; then
          PTHREAD_CXXCPPFLAGS="-D_REENTRANT"
          CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
        fi
        run_this_test=1
      ;;
    esac
  fi

  if test "$run_this_test" = "1" ; then
    AC_LANG_PUSH(C++)
    LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_cxx_success=1, 
                              lam_pthread_cxx_success=0)
    AC_LANG_POP(C++)
    if test "$lam_pthread_cxx_success" = "1"; then
      AC_MSG_RESULT([yes])
    else
      PTHREAD_CXXCPPFLAGS=
      CXXCPPFLAGS="$orig_CXXCPPFLAGS"
      AC_MSG_RESULT([no])
    fi
  fi
fi
])dnl


AC_DEFUN([LAM_INTL_POSIX_THREADS_PLAIN_FC], [
#
# Fortran compiler
#
if test "$lam_pthread_f77_success" = "0" -a "$LAM_WANT_F77_BINDINGS" = "1"; then
  AC_MSG_CHECKING([if F77 compiler and POSIX threads work as is])
  if test "$HAVE_POSIX_THREADS" = "1" ; then
    run_this_test=1
  else
    case "${host_cpu}-${host_os}" in
      *solaris*)
        AC_MSG_RESULT([no - Solaris, not checked])
        run_this_test=0
      ;;
      *)
        run_this_test=1
      ;;
    esac
  fi

  if test "$run_this_test" = "1" ; then
    AC_LANG_PUSH(C)
    LAM_INTL_PTHREAD_TRY_LINK_F77(lam_pthread_f77_success=1, 
                                  lam_pthread_f77_success=0)
    AC_LANG_POP(C)
    if test "$lam_pthread_f77_success" = "1"; then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
    fi
  fi
fi
])dnl


AC_DEFUN([LAM_INTL_POSIX_THREADS_PLAIN], [
# BEGIN: LAM_INTL_POSIX_THREADS_PLAIN
#
# Check if can compile without any special flags
# we throw -D_REENTRANT or -D_THREAD_SAFE in here, just in
# case.  Some systems (OS X, for example) generally don't need
# the defines, but then will on one system header here or there
# why take chances?
#

# Only run C++ and Fortran if those compilers already configured
AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [LAM_INTL_POSIX_THREADS_PLAIN_C],
                  [lam_pthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX], 
                  [LAM_INTL_POSIX_THREADS_PLAIN_CXX], 
                  [lam_pthread_cxx_success=1])

AC_PROVIDE_IFELSE([AC_PROG_F77], 
                  [LAM_INTL_POSIX_THREADS_PLAIN_FC],
                  [lam_pthread_f77_success=1])

# End: LAM_INTL_POSIX_THREADS_PLAIN
])dnl


# ********************************************************************
#
# Try to compile thread support with special compiler flags
#
# ********************************************************************
AC_DEFUN([LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS_C], [
#
# C compiler
#
if test "$lam_pthread_c_success" = "0"; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if C compiler and POSIX threads work with $pf])
    CFLAGS="$orig_CFLAGS $pf"
    AC_LANG_PUSH(C)
    LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_c_success=1,
                              lam_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$lam_pthread_c_success" = "1"; then
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


AC_DEFUN([LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS_CXX], [
#
# C++ compiler
#
if test "$lam_pthread_cxx_success" = "0"; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $pf])
    CXXFLAGS="$orig_CXXFLAGS $pf"
    AC_LANG_PUSH(C++)
    LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_cxx_success=1,
                              lam_pthread_cxx_success=0)
    AC_LANG_POP(C++)
    if test "$lam_pthread_cxx_success" = "1"; then
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


AC_DEFUN([LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS_FC], [
#
# Fortran compiler
#
if test "$lam_pthread_f77_success" = "0" -a "$LAM_WANT_F77_BINDINGS" = "1"; then
  for pf in $pflags; do
    AC_MSG_CHECKING([if F77 compiler and POSIX threads work with $pf])
    FFLAGS="$orig_FFLAGS $pf"
    AC_LANG_PUSH(C)
    LAM_INTL_PTHREAD_TRY_LINK_F77(lam_pthread_f77_success=1, 
                                  lam_pthread_f77_success=0)
    AC_LANG_POP(C)
    if test "$lam_pthread_f77_success" = "1"; then
      PTHREAD_FFLAGS="$pf"
      AC_MSG_RESULT([yes])
      break
    else
      PTHREAD_FFLAGS=
      FFLAGS="$orig_FFLAGS"
      AC_MSG_RESULT([no])
    fi
  done
fi
])


AC_DEFUN([LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS],[
# Begin: LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS
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
pflags="-Kthread -kthread -pthread -pthreads -mt -mthreads"

# Only run C++ and Fortran if those compilers already configured
AC_PROVIDE_IFELSE([AC_PROG_CC], 
                  [LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS_C],
                  [lam_pthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX], 
                  [LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS_CXX], 
                  [lam_pthread_cxx_success=1])

AC_PROVIDE_IFELSE([AC_PROG_F77], 
                  [LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS_FC],
                  [lam_pthread_f77_success=1])

# End: LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS
])dnl


# ********************************************************************
#
# Try to compile thread support with extra libs
#
# ********************************************************************
AC_DEFUN([LAM_INTL_POSIX_THREADS_LIBS_C],[
#
# C compiler
#
if test "$lam_pthread_c_success" = "0"; then
  for pl in $plibs; do
    AC_MSG_CHECKING([if C compiler and POSIX threads work with $pl])
    case "${host_cpu}-${host-_os}" in
      *-aix* | *-freebsd*)
        if test "`echo $CPPFLAGS | grep 'D_THREAD_SAFE'`" = ""; then
          PTRHEAD_CPPFLAGS="-D_THREAD_SAFE"
          CPPFLAGS="$CPPFLAGS $PTHREAD_CPPFLAGS"
        fi
      ;;
      *)
        if test "`echo $CPPFLAGS | grep 'D_REENTRANT'`" = ""; then
          PTHREAD_CPPFLAGS="-D_REENTRANT"
          CPPFLAGS="$CPPFLAGS $PTHREAD_CPPFLAGS"
        fi
      ;;
    esac
    LIBS="$orig_LIBS $pl"
    AC_LANG_PUSH(C)
    LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_c_success=1,
                              lam_pthread_c_success=0)
    AC_LANG_POP(C)
    if test "$lam_pthread_c_success" = "1"; then
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


AC_DEFUN([LAM_INTL_POSIX_THREADS_LIBS_CXX],[
#
# C++ compiler
#
if test "$lam_pthread_cxx_success" = "0"; then
  if test ! "$lam_pthread_c_success" = "0" -a ! "$PTHREAD_LIBS" = "" ; then
    AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $PTHREAD_LIBS])
    case "${host_cpu}-${host-_os}" in
      *-aix* | *-freebsd*)
        if test "`echo $CXXCPPFLAGS | grep 'D_THREAD_SAFE'`" = ""; then
          PTRHEAD_CXXCPPFLAGS="-D_THREAD_SAFE"
          CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
        fi
      ;;
      *)
        if test "`echo $CXXCPPFLAGS | grep 'D_REENTRANT'`" = ""; then
          PTHREAD_CXXCPPFLAGS="-D_REENTRANT"
          CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
        fi
      ;;
    esac
    LIBS="$orig_LIBS $PTHREAD_LIBS"
    AC_LANG_PUSH(C++)
    LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_cxx_success=1, 
                              lam_pthread_cxx_success=0)
    AC_LANG_POP(C++)
    if test "$lam_pthread_cxx_success" = "1"; then
      PTHREAD_LIBS="$pl"
      AC_MSG_RESULT([yes])
    else
      CXXCPPFLAGS="$orig_CXXCPPFLAGS"
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Can not find working threads configuration.  aborting])
    fi
  else 
    for pl in $plibs; do
      AC_MSG_CHECKING([if C++ compiler and POSIX threads work with $pl])
      case "${host_cpu}-${host-_os}" in
        *-aix* | *-freebsd*)
          if test "`echo $CXXCPPFLAGS | grep 'D_THREAD_SAFE'`" = ""; then
            PTRHEAD_CXXCPPFLAGS="-D_THREAD_SAFE"
            CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
          fi
        ;;
        *)
          if test "`echo $CXXCPPFLAGS | grep 'D_REENTRANT'`" = ""; then
            PTHREAD_CXXCPPFLAGS="-D_REENTRANT"
            CXXCPPFLAGS="$CXXCPPFLAGS $PTHREAD_CXXCPPFLAGS"
          fi
        ;;
      esac
      LIBS="$orig_LIBS $pl"
      AC_LANG_PUSH(C++)
      LAM_INTL_PTHREAD_TRY_LINK(lam_pthread_cxx_success=1, 
                                lam_pthread_cxx_success=0)
      AC_LANG_POP(C++)
      if test "$lam_pthread_cxx_success" = "1"; then
	PTHREAD_LIBS="$pl"
        AC_MSG_RESULT([yes])
      else
        PTHREAD_CXXCPPFLAGS=
        CXXCPPFLAGS="$orig_CXXCPPFLAGS"
        AC_MSG_RESULT([no])
      fi
    done
  fi
fi
])dnl


AC_DEFUN([LAM_INTL_POSIX_THREADS_LIBS_FC],[
#
# Fortran compiler
#
if test "$lam_pthread_f77_success" = "0" -a "$LAM_WANT_F77_BINDINGS" = "1"; then
  if test ! "$lam_pthread_c_success" = "0" -a ! "$PTHREAD_LIBS" = "" ; then
    AC_MSG_CHECKING([if F77 compiler and POSIX threads work with $PTHREAD_LIBS])
    LIBS="$orig_LIBS $PTHREAD_LIBS"
    AC_LANG_PUSH(C)
    LAM_INTL_PTHREAD_TRY_LINK_F77(lam_pthread_f77_success=1, 
                                  lam_pthread_f77_success=0)
    AC_LANG_POP(C)
    LIBS="$orig_LIBS"
    if test "$lam_pthread_f77_success" = "1"; then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Can not find working threads configuration.  aborting])
    fi
  else
    for pl in $plibs; do
      AC_MSG_CHECKING([if F77 compiler and POSIX threads work with $pl])
      LIBS="$orig_LIBS $pl"
      AC_LANG_PUSH(C)
      LAM_INTL_PTHREAD_TRY_LINK_F77(lam_pthread_f77_success=1, 
                                    lam_pthread_f77_success=0)
      AC_LANG_POP(C)
      LIBS="$orig_LIBS"
      if test "$lam_pthread_f77_success" = "1"; then
	PTHREAD_LIBS="$pl"
        AC_MSG_RESULT([yes])
        break
      else
        AC_MSG_RESULT([no])
      fi
    done
  fi
fi
])dnl


AC_DEFUN([LAM_INTL_POSIX_THREADS_LIBS],[
# Begin: LAM_INTL_POSIX_THREADS_LIBS
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
                  [LAM_INTL_POSIX_THREADS_LIBS_C], 
                  [lam_pthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX], 
                  [LAM_INTL_POSIX_THREADS_LIBS_CXX], 
                  [lam_pthread_cxx_success=1])

AC_PROVIDE_IFELSE([AC_PROG_F77], 
                  [LAM_INTL_POSIX_THREADS_LIBS_FC],
                  [lam_pthread_f77_success=1])

# End: LAM_INTL_POSIX_THREADS_LIBS]
)dnl


#********************************************************************
#
# External macro (aka, the real thing)
#
#********************************************************************
AC_DEFUN([LAM_CONFIG_POSIX_THREADS],[
lam_pthread_c_success=0
lam_pthread_f77_success=0
lam_pthread_cxx_success=0

orig_CFLAGS="$CFLAGS"
orig_FFLAGS="$FFLAGS"
orig_CXXFLAGS="$CXXFLAGS"
orig_CPPFLAGS="$CPPFLAGS"
orig_CXXCPPFLAGS="$CXXCPPFLAGS"
orig_LDFLAGS="$LDFLAGS"
orig_LIBS="$LIBS"

PTRHEAD_CFLAGS=
PTHREAD_FFLAGS=
PTHREAD_CXXFLAGS=
PTHREAD_CPPFLAGS=
PTHREAD_CXXCPPFLAGS=
PTHREAD_LDFLAGS=
PTHREAD_LIBS=

# Try with the basics, mam.
LAM_INTL_POSIX_THREADS_PLAIN

# Try the super-special compiler flags.
LAM_INTL_POSIX_THREADS_SPECIAL_FLAGS

# Try the normal linking methods (that's no fun)
LAM_INTL_POSIX_THREADS_LIBS

CFLAGS="$orig_CFLAGS"
FFLAGS="$orig_FFLAGS"
CXXFLAGS="$orig_CXXFLAGS"
CPPFLAGS="$orig_CPPFLAGS"
CXXCPPFLAGS="$orig_CXXCPPFLAGS"
LDFLAGS="$orig_LDFLAGS"
LIBS="$orig_LIBS"

if test "$LAM_WANT_F77_BINDINGS" != "1"; then
  lam_pthread_f77_success=1
fi

if test "$lam_pthread_c_success" = "1" -a \
        "$lam_pthread_cxx_success" = "1" -a \
       "$lam_pthread_f77_success" = "1"; then
  internal_useless=1
  $1
else
  internal_useless=1
  $2
fi

unset lam_pthread_c_success lam_pthread_f77_success lam_pthread_cxx_success
unset internal_useless
])dnl
