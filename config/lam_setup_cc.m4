dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([LAM_SETUP_CC],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

lam_show_subtitle "C compiler and preprocessor" 

# $%@#!@#% AIX!!  This has to be called before anything invokes the C
# compiler.

dnl AC_AIX

#
# Check for the compiler
#

lam_cflags_save="$CFLAGS"
AC_PROG_CC
BASECC="`basename $CC`"
CFLAGS="$lam_cflags_save"
AC_DEFINE_UNQUOTED(LAM_CC, "$CC", [LAM underlying C compiler])

# When building LAM, we need this everywhere

CPPFLAGS="$CPPFLAGS -DLAM_BUILDING=1"

# Do we want debugging?

if test "$WANT_DEBUG" = "1"; then
    CFLAGS="$CFLAGS -g"
    LAM_UNIQ(CFLAGS)
    AC_MSG_WARN([-g has been added to CFLAGS (--enable-debug)])
fi

LAM_CFLAGS_BEFORE_PICKY="$CFLAGS"
if test "$GCC" = "yes" -a "$WANT_PICKY_COMPILER" = 1; then
    add="-Wall -Wundef -Wno-long-long"
    add="$add -Wmissing-prototypes -Wstrict-prototypes"
    add="$add -Wcomment -pedantic"

    # see if -Wno-long-double works...
    CFLAGS_orig="$CFLAGS"
    CFLAGS="$CFLAGS -Wno-long-double"
    AC_TRY_COMPILE([], [], add="$add -Wno-long-double")
    CFLAGS="$CFLAGS_orig"

    add="$add -Werror-implicit-function-declaration "

    CFLAGS="$CFLAGS $add"
    LAM_UNIQ(CFLAGS)
    AC_MSG_WARN([$add has been added to CFLAGS (developer copy)])
    unset add
fi

# See if this version of gcc allows -finline-functions
if test "$GCC" = "yes"; then
    CFLAGS_orig="$CFLAGS"
    CFLAGS="$CFLAGS -finline-functions"
    add=
    AC_TRY_COMPILE([], [], add=" -finline-functions")
    CFLAGS="$CFLAGS_orig$add"
    LAM_UNIQ(CFLAGS)
    AC_MSG_WARN([$add has been added to CFLAGS])
    unset add
fi

# Preload the optflags for the case where the user didn't specify any.
# If we're using GNU compilers, use -O3 (since it GNU doesn't require
# all compilation units to be compiled with the same level of
# optimization -- selecting a high level of optimization is not
# prohibitive).  If we're using anything else, be conservative and
# just use -O.  

if test "$GCC" = yes; then
    OPTFLAGS="-O3"
else
    OPTFLAGS="-O"
fi

LAM_CHECK_OPTFLAGS("$LAM_CFLAGS_BEFORE_PICKY")
LAM_CFLAGS_BEFORE_PICKY="$co_result"

AC_MSG_CHECKING([for C optimization flags])
LAM_CHECK_OPTFLAGS("$CFLAGS")
AC_MSG_RESULT([$co_result])
CFLAGS="$co_result"])
