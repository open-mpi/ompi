# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_INIT([plpa], 
        [m4_normalize(esyscmd([config/plpa_get_version.sh VERSION --base]))],
        [http://www.open-mpi.org/community/help/], [plpa])
AC_PREREQ(2.59)
AC_CONFIG_AUX_DIR(./config)
AC_CONFIG_MACRO_DIR(./config)

# Init automake
AM_INIT_AUTOMAKE([foreign dist-bzip2 subdir-objects no-define])

# Make configure depend on the VERSION file, since it's used in AC_INIT
AC_SUBST([CONFIGURE_DEPENDENCIES], ['$(top_srcdir)/VERSION'])

# Get the version of OMPI that we are installing
AC_MSG_CHECKING([for PLPA version])
PLPA_VERSION="`$srcdir/config/plpa_get_version.sh $srcdir/VERSION`"
PLPA_MAJOR_VERSION="`$srcdir/config/plpa_get_version.sh $srcdir/VERSION --major`"
PLPA_MINOR_VERSION="`$srcdir/config/plpa_get_version.sh $srcdir/VERSION --minor`"
PLPA_RELEASE_VERSION="`$srcdir/config/plpa_get_version.sh $srcdir/VERSION --release`"
PLPA_SVN_R="`$srcdir/config/plpa_get_version.sh $srcdir/VERSION --svn`"
AC_SUBST(PLPA_VERSION)
AC_SUBST(PLPA_SVN_R)
AC_DEFINE_UNQUOTED([PLPA_MAJOR_VERSION], [$PLPA_MAJOR_VERSION],
    [Major version of PLPA])
AC_DEFINE_UNQUOTED([PLPA_MINOR_VERSION], [$PLPA_MINOR_VERSION],
    [Minor version of PLPA])
AC_DEFINE_UNQUOTED([PLPA_RELEASE_VERSION], [$PLPA_RELEASE_VERSION],
    [Release version of PLPA])
AC_MSG_RESULT([$PLPA_VERSION])

# override/fixup the version numbers set by AC_INIT, since on
# developer builds, there's no good way to know what the version is
# before running configure :(.  We only use the base version number
# (ie, no svn r numbers) for the version set in AC_INIT.  This will
# always match reality because we add the VERSION file (the only way
# to change the major.minor.release{greek}) into the configure
# dependencies.
PACKAGE_VERSION="$PLPA_VERSION"
PACKAGE_STRING="${PACKAGE_NAME} ${PACKAGE_VERSION}"
VERSION="${PACKAGE_VERSION}"

# For standalone configurations, we include a .so version number.

. $srcdir/VERSION
AC_SUBST([libplpa_so_version])

# Setup the header file
AH_TOP([/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef PLPA_CONFIG_H
#define PLPA_CONFIG_H
])
AH_BOTTOM([
#endif /* PLPA_CONFIG_H */
])

# Compiler stuff
CFLAGS_save="$CFLAGS"
AC_PROG_CC
AM_PROG_CC_C_O
CFLAGS="$CFLAGS_save"

# For the common developer case, if we're in a Subversion checkout and
# using the GNU compilers, turn on maximum warnings unless
# specifically disabled by the user.
AC_MSG_CHECKING([whether to enable "picky" compiler mode])
want_picky=0
if test -d "$srcdir/.svn" -a "$GCC" = "yes"; then
    want_picky=1
fi
AC_ARG_ENABLE(picky,
              AC_HELP_STRING([--disable-picky],
                             [When in Subversion checkouts of PLPA and compiling with gcc, the default is to enable maximum compiler pickyness.  Using --disable-picky or --enable-picky overrides any default setting]))
if test "$enable_picky" = "yes"; then
    if test "$GCC" = "yes"; then
        AC_MSG_RESULT([yes])
        want_picky=1
    else
        AC_MSG_RESULT([no])
        AC_MSG_WARN([Warning: --enable-picky used, but is currently only defined for the GCC compiler set -- automatically disabled])
        want_picky=0
    fi
elif test "$enable_picky" = "no"; then
    AC_MSG_RESULT([no])
    want_picky=0
else
    if test "$want_picky" = 1; then
        AC_MSG_RESULT([yes (default)])
    else
        AC_MSG_RESULT([no (default)])
    fi
fi
if test "$want_picky" = 1; then
    add="-Wall -Wundef -Wno-long-long -Wsign-compare"
    add="$add -Wmissing-prototypes -Wstrict-prototypes"
    add="$add -Wcomment -pedantic"

    CFLAGS="$CFLAGS $add"
fi

# Setup the PLPA (taking all defaults)
PLPA_INIT([.], [plpa_setup=happy], [plpa_setup=unhappy])
AS_IF([test "$plpa_setup" = "unhappy"],
      [AC_MSG_ERROR([Cannot continue])])

if test "$plpa_debug" = "1"; then
    CFLAGS="$CFLAGS -g"
fi

# Setup libtool
AM_ENABLE_SHARED
AM_DISABLE_STATIC
AM_PROG_LIBTOOL

# Party on
AC_OUTPUT
