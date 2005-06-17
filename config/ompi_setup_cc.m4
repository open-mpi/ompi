dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_SETUP_CC],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

ompi_show_subtitle "C compiler and preprocessor" 

# $%@#!@#% AIX!!  This has to be called before anything invokes the C
# compiler.

dnl AC_AIX

#
# Check for the compiler
#

ompi_cflags_save="$CFLAGS"
AC_PROG_CC
BASECC="`basename $CC`"
CFLAGS="$ompi_cflags_save"
AC_DEFINE_UNQUOTED(OMPI_CC, "$CC", [OMPI underlying C compiler])
OMPI_CC_ABSOLUTE="`which $CC`"
AC_SUBST(OMPI_CC_ABSOLUTE)

# Check for compilers that impersonate gcc

AC_MSG_CHECKING([for compilers that impersonate gcc])
msg=
TRULY_GCC=$GCC
if test "$GCC" = "yes"; then
    AC_TRY_COMPILE([], [
int i = 3;
#if __INTEL_COMPILER
#error Yes, I am lying about being gcc.
#endif
], [], [msg=intel])

    # If we made it through unscathed, then it really is gcc
    if test -z "$msg"; then
        TRULY_GCC=yes
    else
        TRULY_GCC=no
    fi
else
    # We never thought that this was gcc to begin with
    msg="not applicable"
    TRULY_GCC=no
fi
AC_MSG_RESULT([$msg])

# Do we want code coverage
if test "$WANT_COVERAGE" = "1"; then 
     if test "$TRULY_GCC" = "yes"; then 
         CLEANFILES="*.bb *.bbg *.da *.*.gcov ${CLEANFILES}"
         AC_MSG_WARN([-fprofile-arcs -ftest-coverage has been added to CFLAGS (--enable-coverage)])
         WANT_DEBUG=1
         CFLAGS="-ftest-coverage -fprofile-arcs ${CFLAGS}"
         WRAPPER_EXTRA_CFLAGS="-ftest-coverage -fprofile-arcs ${WRAPPER_EXTRA_CFLAGS}"
      else
         AC_MSG_WARN([Code coverage functionality is currently available only with GCC])
         AC_MSG_ERROR([Configure: Cannot continue])
      fi
fi

# Do we want debugging?

if test "$WANT_DEBUG" = "1"; then
    CFLAGS="$CFLAGS -g"
    OMPI_UNIQ(CFLAGS)
    AC_MSG_WARN([-g has been added to CFLAGS (--enable-debug)])
fi

# These flags are generally gcc-specific; even the gcc-impersonating
# compilers won't accept them.

OMPI_CFLAGS_BEFORE_PICKY="$CFLAGS"
if test "$TRULY_GCC" = "yes" -a "$WANT_PICKY_COMPILER" = 1; then
    add="-Wall -Wundef -Wno-long-long -Wsign-compare"
    add="$add -Wmissing-prototypes -Wstrict-prototypes"
    add="$add -Wcomment -pedantic"

    # see if -Wno-long-double works...
    CFLAGS_orig="$CFLAGS"
    CFLAGS="$CFLAGS -Wno-long-double"
    AC_TRY_COMPILE([], [], add="$add -Wno-long-double")
    CFLAGS="$CFLAGS_orig"

    add="$add -Werror-implicit-function-declaration "

    CFLAGS="$CFLAGS $add"
    OMPI_UNIQ(CFLAGS)
    AC_MSG_WARN([$add has been added to CFLAGS (--enable-picky)])
    unset add
fi

# See if this version of gcc allows -finline-functions and/or
# -fno-strict-aliasing.  Even check the gcc-impersonating compilers.
if test "$GCC" = "yes"; then
    CFLAGS_orig="$CFLAGS"

    CFLAGS="$CFLAGS_orig -finline-functions"
    add=
    AC_TRY_COMPILE([], [], add=" -finline-functions")
    CFLAGS="$CFLAGS_orig$add"

    CFLAGS="$CFLAGS_orig -fno-strict-aliasing"
    add=
    AC_TRY_COMPILE([], [], add=" -fno-strict-aliasing")
    CFLAGS="$CFLAGS_orig$add"

    OMPI_UNIQ(CFLAGS)
    AC_MSG_WARN([$add has been added to CFLAGS])
    unset add
fi

# Try to enable restrict keyword
RESTRICT_CFLAGS=
case "${host}" in
    ia64-unknown-linux*)
        if test "$CC" = "ecc" ; then
            RESTRICT_CFLAGS="-restrict"
        fi
    ;;
    mips-sgi-irix*)
        if test "$CC" = "cc" ; then
            RESTRICT_CFLAGS="-LANG:restrict=ON"
        fi
    ;;
    i?86-pc-linux*)
        if test "$CC" = "icc" ; then
            RESTRICT_CFLAGS="-restrict"
        fi
    ;;
esac
if test ! -z "$RESTRICT_CFLAGS" ; then
    CFLAGS_orig="$CFLAGS"
    CFLAGS="$CFLAGS_orig $RESTRICT_CFLAGS"
    add=
    AC_TRY_COMPILE([], [], add=" $RESTRICT_CFLAGS")
    CFLAGS="${CFLAGS_orig}${add}"
    OMPI_UNIQ(CFLAGS)
    AC_MSG_WARN([$add has been added to CFLAGS])
    unset add
fi

# Preload the optflags for the case where the user didn't specify any.
# If we're using GNU compilers, use -O3 (since it GNU doesn't require
# all compilation units to be compiled with the same level of
# optimization -- selecting a high level of optimization is not
# prohibitive).  If we're using anything else, be conservative and
# just use -O.  

# Note: gcc-imperonating compilers accept -O3, so there's no need for
# $TRULY_GCC here.

if test "$GCC" = yes; then
    OPTFLAGS="-O3"
else
    OPTFLAGS="-O"
fi

OMPI_CHECK_OPTFLAGS("$OMPI_CFLAGS_BEFORE_PICKY")
OMPI_CFLAGS_BEFORE_PICKY="$co_result"

AC_MSG_CHECKING([for C optimization flags])
OMPI_CHECK_OPTFLAGS("$CFLAGS")
AC_MSG_RESULT([$co_result])
CFLAGS="$co_result"])
