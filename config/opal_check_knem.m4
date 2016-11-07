dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2009      The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2014-2016 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_CHECK_KNEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if knem support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OPAL_CHECK_KNEM],[
    if test -z "$opal_check_knem_happy" ; then
	OPAL_VAR_SCOPE_PUSH([opal_check_knem_$1_save_CPPFLAGS opal_check_knem_dir])
	AC_ARG_WITH([knem],
		    [AC_HELP_STRING([--with-knem(=DIR)],
				    [Build knem Linux kernel module support, searching for headers in DIR/include])])

	OPAL_CHECK_WITHDIR([knem], [$with_knem], [include/knem_io.h])
	opal_check_knem_$1_save_CPPFLAGS="$CPPFLAGS"

	opal_check_knem_happy=no

	AS_IF([test "$with_knem" != "no"],
	      [AS_IF([test ! -z "$with_knem" && test "$with_knem" != "yes"],
		     [opal_check_knem_dir="$with_knem"])

	       _OPAL_CHECK_PACKAGE_HEADER([ompi_check_knem],
					  [knem_io.h],
					  [$opal_check_knem_dir],
					  [opal_check_knem_happy="yes"],
					  [])],
	      [])

	CPPFLAGS="$CPPFLAGS $ompi_check_knem_CPPFLAGS"

	# need at least version 0x0000000b
	if test "$opal_check_knem_happy" = "yes" ; then
	    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
#include <knem_io.h>
					      ],[
#if KNEM_ABI_VERSION < 0xc
#error "Version less than 0xc"
#endif
					      ])],
			      [opal_check_knem_happy=yes],
			      [opal_check_knem_happy=no])
	fi

	CPPFLAGS="$opal_check_knem_$1_save_CPPFLAGS"

        OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/Linux KNEM]],[$1],[$opal_check_knem_happy])
	OPAL_VAR_SCOPE_POP
    fi

    AS_IF([test "$opal_check_knem_happy" = "yes"],
          [$1_CPPFLAGS="[$]$1_CPPFLAGS $ompi_check_knem_CPPFLAGS"
	   $2],
          [AS_IF([test ! -z "$with_knem" && test "$with_knem" != "no"],
                 [AC_MSG_ERROR([KNEM support requested but not found.  Aborting])])
           $3])
])dnl
