dnl
dnl $HEADER$
dnl
dnl LAM_CONFIG_SOLARIS_THREADS()
dnl

# ********************************************************************
#
# Internal macros - do not call from outside LAM_CONFIG_SOLARIS_THREADS
#
# ********************************************************************

AC_DEFUN([LAM_INTL_SOLARIS_TRY_LINK], [
# BEGIN: LAM_INTL_SOLARIS_TRY_LINK
#
# Make sure that we can run a small application in C or C++, which
# ever is the current language.  Do make sure that C or C++ is the
# current language.
    AC_TRY_LINK([#include <thread.h>],
                 [thread_t th; thr_join(th, 0, 0);
                 thr_create(0,0,0,0,0,0); ],
                 [$1], [$2])
# END: LAM_INTL_SOLARIS_TRY_LINK
])dnl


AC_DEFUN([LAM_INTL_SOLARIS_TRY_LINK_F77], [
# BEGIN: LAM_INTL_SOLARIS_TRY_LINK_F77
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
#include <thread.h>
$lam_conftest_h

#ifdef __cplusplus
extern "C" {
#endif
void lam_pthread()
{
  thread_t th;
  thr_join(th, 0, 0);
  thr_create(0,0,0,0,0,0);
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
# END: LAM_INTL_SOLARIS_TRY_LINK_F77
])dnl


AC_DEFUN([LAM_CONFIG_SOLARIS_THREADS_C], [
if test "$BASECC" = "cc"; then
    STHREAD_CFLAGS="-mt"
    style="Workshop/Forte"
else
    STHREAD_CPPFLAGS="-D_REENTRANT"
    STHREAD_LIBS="-lthread"
    style="-lthread"
fi
AC_MSG_CHECKING([if C compiler and Solaris threads work])
CFLAGS="$STHREAD_CFLAGS $CFLAGS_orig"
CPPFLAGS="$STHREAD_CPPFLAGS $CPPFLAGS_orig"
LDFLAGS="$STHREAD_LDFLAGS $LDFLAGS_orig"
LIBS="$STHREAD_LIBS $LIBS_orig"
AC_LANG_PUSH(C)
LAM_INTL_SOLARIS_TRY_LINK(lam_sthread_c_success=1,
                          lam_sthread_c_success=0)
AC_LANG_POP(C)
if test "$lam_sthread_c_success" = "1"; then
    AC_MSG_RESULT([yes - $style])
else
    AC_MSG_RESULT([no])
fi
])dnl


AC_DEFUN([LAM_CONFIG_SOLARIS_THREADS_CXX], [
if test "$BASECXX" = "CC"; then
    STHREAD_CXXFLAGS="-mt"
    style="Workshop/Forte"
elif test "$BASECXX" = "KCC"; then
    STHREAD_CXXFLAGS="--backend -mt"
    style="KCC"
else
    STHREAD_CXXCPPFLAGS="-D_REENTRANT"
    STHREAD_LIBS="-lthread"
    style="-lthread"
fi
CXXFLAGS="$STHREAD_CXXFLAGS $CXXFLAGS_orig"
CXXCPPFLAGS="$STHREAD_CXXPPFLAGS $CXXPPFLAGS_orig"
LDFLAGS="$STHREAD_LDFLAGS $LDFLAGS_orig"
LIBS="$STHREAD_LIBS $LIBS_orig"
AC_MSG_CHECKING([if C++ compiler and Solaris threads work])
AC_LANG_PUSH(C++)
LAM_INTL_SOLARIS_TRY_LINK(lam_sthread_cxx_success=1, 
                          lam_sthread_cxx_success=0)
AC_LANG_POP(C++)
if test "$lam_sthread_cxx_success" = "1"; then
    AC_MSG_RESULT([yes - $style])
else
    AC_MSG_RESULT([no])
fi
])dnl


AC_DEFUN([LAM_CONFIG_SOLARIS_THREADS_FC], [
if test "$LAM_WANT_FORTRAN" = "1"; then
    if test "$BASEFC" = "f77"; then
        STHREAD_FFLAGS="-mt"
        style="Workshop/Forte"
    else
        STHREAD_LIBS="-lthread"
        style="-lthread"
    fi
    FFLAGS="$STHREAD_FFLAGS $FFLAGS_orig"
    CFLAGS="$STHREAD_CFLAGS $CFLAGS_orig"
    CPPFLAGS="$STHREAD_CPPFLAGS $CPPFLAGS_orig"
    LDFLAGS="$STHREAD_LDFLAGS $LDFLAGS_orig"
    LIBS="$STHREAD_LIBS $LIBS_orig"
    AC_MSG_CHECKING([if F77 compiler and Solaris threads work])
    AC_LANG_PUSH(C)
    LAM_INTL_SOLARIS_TRY_LINK_F77(lam_sthread_f77_success=1, 
                                  lam_sthread_f77_success=0)
    AC_LANG_POP(C)
    if test "$lam_sthread_f77_success" = "1"; then
        AC_MSG_RESULT([yes - $style])
     else
        AC_MSG_RESULT([no])
     fi
else
  lam_sthread_f77_success=1
fi
])dnl


AC_DEFUN([LAM_CONFIG_SOLARIS_THREADS],[
lam_sthread_c_success=0
lam_sthread_f77_success=0
lam_sthread_cxx_success=0

orig_CFLAGS="$CFLAGS"
orig_FFLAGS="$FFLAGS"
orig_CXXFLAGS="$CXXFLAGS"
orig_CPPFLAGS="$CPPFLAGS"
orig_CXXCPPFLAGS="$CXXCPPFLAGS"
orig_LDFLAGS="$LDFLAGS"
orig_LIBS="$LIBS"

STHREAD_CFLAGS=
STHREAD_FFLAGS=
STHREAD_CXXFLAGS=
STHREAD_CPPFLAGS=
STHREAD_CXXCPPFLAGS=
STHREAD_LDFLAGS=
STHREAD_LIBS=

# Only run C++ and Fortran if those compilers already configured
AC_PROVIDE_IFELSE([AC_PROG_CC], 
                  [LAM_CONFIG_SOLARIS_THREADS_C],
                  [lam_sthread_c_success=1])

AC_PROVIDE_IFELSE([AC_PROG_CXX], 
                  [LAM_CONFIG_SOLARIS_THREADS_CXX],
                  [lam_sthread_cxx_success=1])

AC_PROVIDE_IFELSE([LAM_PROG_F77], 
                  [LAM_CONFIG_SOLARIS_THREADS_FC],
                  [lam_sthread_f77_success=1])

CFLAGS="$orig_CFLAGS"
FFLAGS="$orig_FFLAGS"
CXXFLAGS="$orig_CXXFLAGS"
CPPFLAGS="$orig_CPPFLAGS"
CXXCPPFLAGS="$orig_CXXCPPFLAGS"
LDFLAGS="$orig_LDFLAGS"
LIBS="$orig_LIBS"

if test "$lam_sthread_c_success" = "1" -a \
        "$lam_sthread_cxx_success" = "1" -a \
       "$lam_sthread_f77_success" = "1"; then
  internal_useless=1
  $1
else
  internal_useless=1
  $2
fi

unset lam_sthread_c_success lam_sthread_f77_success lam_sthread_cxx_success
unset internal_useless
])dnl

