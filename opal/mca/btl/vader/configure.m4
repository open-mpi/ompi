# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_CHECK_XPMEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if XPMEM support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OPAL_CHECK_XPMEM], [
    OPAL_VAR_SCOPE_PUSH([opal_check_xpmem_happy])
    AC_ARG_WITH([xpmem],
                [AC_HELP_STRING([--with-xpmem(=DIR)],
                [Build with XPMEM kernel module support, searching for headers in DIR])])
    OPAL_CHECK_WITHDIR([xpmem], [$with_xpmem], [include/xpmem.h])

    AC_ARG_WITH([xpmem-libdir],
                [AC_HELP_STRING([--with-xpmem-libdir=DIR],
                                [Search for XPMEM library in DIR])])
    OPAL_CHECK_WITHDIR([xpmem-libdir], [$with_xpmem_libdir], [libxpmem.*])

    opal_check_xpmem_happy="no"

    if test ! "$with_xpmem" = "no" ; then
	if test ! -z "$with_xpmem" -a "$with_xpmem" != "yes" ; then
	    opal_check_xpmem_dir="$with_xpmem"
	fi

	if test ! -z "$with_xpmem_libdir" -a "$with_xpmem_libdir" != "yes" ; then
	    opal_check_xpmem_libdir="$with_xpmem_libdir"
	fi

	OPAL_CHECK_PACKAGE([$1],[xpmem.h],[xpmem],[xpmem_make],[],
	    [$opal_check_xpmem_dir],[$opal_check_xpmem_libdir], [opal_check_xpmem_happy="yes"], [])

	if test "$opal_check_xpmem_happy" = "no" -a -n "$with_xpmem" -a "$with_xpmem" != "yes" ; then
	    AC_MSG_ERROR([XPMEM support requested but not found.  Aborting])
	fi
    fi

    AS_IF([test "$opal_check_xpmem_happy" = "yes"], [$2], [$3])

    OPAL_VAR_SCOPE_POP
])dnl

# MCA_btl_vader_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_vader_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/vader/Makefile])

    OPAL_VAR_SCOPE_PUSH([btl_vader_xpmem_happy btl_vader_cma_happy btl_vader_knem_happy])

    # Check for single-copy APIs

    OPAL_CHECK_CRAY_XPMEM([btl_vader], [btl_vader_xpmem_happy=1], [btl_vader_xpmem_happy=0])

    AS_IF([test "$btl_vader_xpmem_happy" -eq 0],
          [OPAL_CHECK_XPMEM([btl_vader], [btl_vader_xpmem_happy=1], [btl_vader_xpmem_happy=0])],[])

    OPAL_CHECK_KNEM([btl_vader], [btl_vader_knem_happy=1],[btl_vader_knem_happy=0])
    OPAL_CHECK_CMA([btl_vader], [AC_CHECK_HEADER([sys/prctl.h]) btl_vader_cma_happy=1], [btl_vader_cma_happy=0])

    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_HAVE_XPMEM], [$btl_vader_xpmem_happy],
        [If XPMEM support can be enabled within vader])

    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_HAVE_CMA], [$btl_vader_cma_happy],
        [If CMA support can be enabled within vader])

    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_HAVE_KNEM], [$btl_vader_knem_happy],
	[If KNEM support can be enabled within vader])

    OPAL_VAR_SCOPE_POP

    # always happy
    [$1]

    # substitute in the things needed to build with XPMEM support
    AC_SUBST([btl_vader_CFLAGS])
    AC_SUBST([btl_vader_CPPFLAGS])
    AC_SUBST([btl_vader_LDFLAGS])
    AC_SUBST([btl_vader_LIBS])
])dnl
