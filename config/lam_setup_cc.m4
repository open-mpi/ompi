dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl
dnl $Id: lam_setup_cc.m4,v 1.2 2004/01/07 08:31:09 jsquyres Exp $
dnl

AC_DEFUN(LAM_SETUP_CC,[

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
    if test "$GCC" = "yes"; then
	add="-g -Wall -Wundef -Wno-long-long"
	add="$add -Wmissing-prototypes -Wstrict-prototypes"

        # see if -Wno-long-double works...
        CFLAGS_orig="$CFLAGS"
        CFLAGS="$CFLAGS -Wno-long-double"
        AC_TRY_COMPILE([], [], add="$add -Wno-long-double")
        CFLAGS="$CFLAGS_orig"

        add="$add -Werror-implicit-function-declaration "
    else
	add="-g"
    fi
    CFLAGS="$CFLAGS $add"
    LAM_UNIQ(CFLAGS)
    AC_MSG_WARN([$add has been added to CFLAGS (--with-debug)])
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
LAM_CHECK_OPTFLAGS("$CFLAGS")
AC_MSG_CHECKING([for C optimization flags])
AC_MSG_RESULT([$co_result])
CFLAGS="$co_result"
])
