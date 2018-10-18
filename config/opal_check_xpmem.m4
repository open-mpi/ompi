# -*- shell-script ; indent-tabs-mode:nil -*-
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
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# Copyright (c) 2014-2015 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# special check for cray xpmem, uses macro(s) from pkg.m4
#
# OPAL_CHECK_CRAY_XPMEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_CRAY_XPMEM],[
    if test -z "$opal_check_cray_xpmem_happy" ; then
        AC_ARG_WITH([cray_xpmem],
                    [AC_HELP_STRING([--with-cray-xpmem(=yes/no)],
                    [Build Cray XPMEM support(default: auto)])],
                    [], with_cray_xpmem=auto)

       AC_MSG_CHECKING([for Cray XPMEM support])
       AS_IF([test "$with_cray_xpmem" = "no"],
             [AC_MSG_RESULT([no])
              $3],
             [AS_IF([test "$with_cray_xpmem" = "auto" || test "$with_cray_xpmem" = "yes"],
                     [PKG_CHECK_MODULES_STATIC([CRAY_XPMEM], [cray-xpmem],
                                        [opal_check_cray_xpmem_happy="yes"],
                                        [opal_check_cray_xpmem_happy="no"]
                                        [AS_IF([test "$with_cray_xpmem" = "yes"],
                                               [AC_MSG_WARN([Cray XPMEM support requested but pkg-config failed.])
                                                AC_MSG_ERROR([Aborting])],[])]
                                         )],
                     [])
             ])

        AS_IF([test "$opal_check_cray_xpmem_happy" = "yes" && test "$enable_static" = "yes"],
              [CRAY_XPMEM_LIBS = $CRAY_XPMEM_STATIC_LIBS],[])
    fi

    AS_IF([test "$opal_check_cray_xpmem_happy" = "yes"],
          [$1_LDFLAGS="[$]$1_LDFLAGS $CRAY_XPMEM_LIBS"
           $1_CPPFLAGS="[$]$1_CPPFLAGS $CRAY_XPMEM_CFLAGS"
           $1_LIBS="[$]$1_LIBS $CRAY_XPMEM_LIBS"
           AC_DEFINE_UNQUOTED([HAVE_XPMEM_H], [1],[is xpmem.h available])
           $2], [$3])
])

# OPAL_CHECK_XPMEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if XPMEM support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OPAL_CHECK_XPMEM], [
    if test -z "$opal_check_xpmem_happy" ; then
	# check for a cray installed xpmem first
	OPAL_CHECK_CRAY_XPMEM([opal_check_xpmem],[opal_check_xpmem_happy=yes],[opal_check_xpmem_happy=no])

	if test "$opal_check_xpmem_happy" = no ; then
	    AC_ARG_WITH([xpmem],
			[AC_HELP_STRING([--with-xpmem(=DIR)],
					[Build with XPMEM kernel module support, searching for headers in DIR])])
	    OPAL_CHECK_WITHDIR([xpmem], [$with_xpmem], [include/xpmem.h])

	    AC_ARG_WITH([xpmem-libdir],
			[AC_HELP_STRING([--with-xpmem-libdir=DIR],
					[Search for XPMEM library in DIR])])
	    OPAL_CHECK_WITHDIR([xpmem-libdir], [$with_xpmem_libdir], [libxpmem.*])

	    if test ! "$with_xpmem" = "no" ; then
		if test ! -z "$with_xpmem" && test "$with_xpmem" != "yes" ; then
		    opal_check_xpmem_dir="$with_xpmem"
		fi

		if test ! -z "$with_xpmem_libdir" && test "$with_xpmem_libdir" != "yes" ; then
		    opal_check_xpmem_libdir="$with_xpmem_libdir"
		fi

		OPAL_CHECK_PACKAGE([opal_check_xpmem],[xpmem.h],[xpmem],[xpmem_make],[],
				   [$opal_check_xpmem_dir],[$opal_check_xpmem_libdir], [opal_check_xpmem_happy="yes"], [])

		if test "$opal_check_xpmem_happy" = "no" && test -n "$with_xpmem" && test "$with_xpmem" != "yes" ; then
		    AC_MSG_ERROR([XPMEM support requested but not found.  Aborting])
		fi
	    fi
	fi

	OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/XPMEM]],[$1],[$opal_check_xpmem_happy])
    fi

    AS_IF([test "$opal_check_xpmem_happy" = "yes"], [
	      $1_CPPFLAGS="[$]$1_CPPFLAGS $opal_check_xpmem_CPPFLAGS"
	      $1_LDFLAGS="[$]$1_LDFLAGS $opal_check_xpmem_LDFLAGS"
	      $1_LIBS="[$]$1_LIBS $opal_check_xpmem_LIBS"
	      $2], [$3])
])dnl
