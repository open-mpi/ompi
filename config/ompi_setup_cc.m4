dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
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

# When building OMPI, we need this everywhere

CPPFLAGS="$CPPFLAGS -DOMPI_BUILDING=1"

# Do we want debugging?

if test "$WANT_DEBUG" = "1"; then
    CFLAGS="$CFLAGS -g"
    OMPI_UNIQ(CFLAGS)
    AC_MSG_WARN([-g has been added to CFLAGS (--enable-debug)])
fi

OMPI_CFLAGS_BEFORE_PICKY="$CFLAGS"
if test "$GCC" = "yes" -a "$WANT_PICKY_COMPILER" = 1; then
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

# See if this version of gcc allows -finline-functions
if test "$GCC" = "yes"; then
    CFLAGS_orig="$CFLAGS"
    CFLAGS="$CFLAGS -finline-functions"
    add=
    AC_TRY_COMPILE([], [], add=" -finline-functions")
    CFLAGS="$CFLAGS_orig$add"
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
