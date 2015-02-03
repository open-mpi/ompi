dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2012 University of Houston. All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_PLFS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PLFS support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_PLFS],[

    check_plfs_CPPFLAGS=
    check_plfs_LDFLAGS=
    check_plfs_LIBS=

    check_plfs_save_LIBS="$LIBS" 
    check_plfs_save_LDFLAGS="$LDFLAGS"
    check_plfs_save_CPPFLAGS="$CPPFLAGS"

    check_plfs_configuration="none"
    ompi_check_plfs_happy="yes"


    # Get some configuration information
    AC_ARG_WITH([plfs],
        [AC_HELP_STRING([--with-plfs(=DIR)],
             [Build Plfs support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([plfs], [$with_plfs], [include/plfs.h])

    AC_ARG_WITH([plfs-libs], 
        [AC_HELP_STRING([--with-plfs-libs=LIBS],
                       [Libraries to link with for plfs])])

    temp_with_plfs="$with_plfs"
    AS_IF([test -z "$with_plfs"],
          [with_plfs="/usr/local"])

    temp_with_plfs_libs="$with_plfs_libs"
    AS_IF([test -z "$with_plfs_libs"],
	[with_plfs_libs="plfs pthread"])
    
    # Add correct -I and -L flags
    AS_IF([test -d "$with_plfs/include"],
        [check_plfs_CPPFLAGS="-I$with_plfs/include"
            $1_CPPFLAGS="$check_plfs_CPPFLAGS"
            CPPFLAGS="$CPPFLAGS $check_plfs_CPPFLAGS"],
	[ompi_check_plfs_happy="no"])
    
    AS_IF([test "$ompi_check_plfs_happy" = "yes"],
	[AS_IF([test -d "$with_plfs/lib"],
		[check_plfs_LDFLAGS="-L$with_plfs/lib"
		    $1_LDFLAGS="$check_plfs_LDFLAGS"
		    LDFLAGS="$LDFLAGS $check_plfs_LDFLAGS"],
		[ompi_check_plfs_happy="no"]) 
    ],[])
	    
    # Try to find all the plfs libraries
    AS_IF([test "$ompi_check_plfs_happy" = "yes"],
	[ AS_IF([test -n "$with_plfs_libs"]
		[for lib in $with_plfs_libs ; do
		    check_plfs_LIBS="$check_plfs_LIBS -l$lib"
		    done]) 
		
	    $1_LIBS="$check_plfs_LIBS"
	    LIBS="$LIBS $check_plfs_LIBS"

            # check for plfs
	    AC_CHECK_HEADERS([plfs.h],
		[AC_MSG_CHECKING([if possible to link PLFS])
		    AC_LINK_IFELSE(
			[AC_LANG_PROGRAM(
				[[#include <plfs.h>]],
				[[is_plfs_path(NULL);]])],
			[AC_MSG_RESULT([yes])
			    ompi_check_plfs_happy="yes"],
			[AC_MSG_RESULT([no])
			    ompi_check_plfs_happy="no"])],
		[ompi_check_plfs_happy="no"])
    ])


    LDFLAGS="$check_plfs_save_LDFLAGS"
    CPPFLAGS="$check_plfs_save_CPPFLAGS"
    LIBS="$check_plfs_save_LIBS"
    AS_IF([test "$ompi_check_plfs_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_plfs" && test "$with_plfs" != "no"],
                  [echo PLFS support not found])
              $3])

    with_plfs="$temp_with_plfs"
    with_plfs_libs="$temp_with_plfs_libs"

])

