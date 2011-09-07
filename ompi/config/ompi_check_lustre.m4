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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008-2011 University of Houston. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_LUSTRE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if LUSTRE support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_LUSTRE],[

    check_lustre_CPPFLAGS=
    check_lustre_LDFLAGS=
    check_lustre_LIBS=

    check_lustre_save_wrapper_extra_libs="$WRAPPER_EXTRA_LIBS"
    check_lustre_save_wrapper_extra_ldflags="$WRAPPER_EXTRA_LDFLAGS"
    check_lustre_save_wrapper_extra_cflags="$WRAPPER_EXTRA_CFLAGS"
    check_lustre_save_wrapper_extra_cppflags="$WRAPPER_EXTRA_CPPFLAGS"
    check_lustre_save_LIBS="$LIBS" 
    check_lustre_save_LDFLAGS="$LDFLAGS"
    check_lustre_save_CFLAGS="$CFLAGS"
    check_lustre_save_CPPFLAGS="$CPPFLAGS"

    check_lustre_configuration="none"
    ompi_check_lustre_happy="yes"


    # Get some configuration information
    AC_ARG_WITH([lustre],
        [AC_HELP_STRING([--with-lustre(=DIR)],
             [Build Lustre support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([lustre], [$with_lustre], [include/lustre/liblustreapi.h])

    AC_ARG_WITH([lustre-libs], 
        [AC_HELP_STRING([--with-lustre-libs=LIBS],
                       [Libraries to link with for lustre])])

    temp_with_lustre="$with_lustre"
    AS_IF([test -z "$with_lustre"],
          [with_lustre="/usr/local"])

    temp_with_lustre_libs="$with_lustre_libs"
    AS_IF([test -z "$with_lustre_libs"],
	[with_lustre_libs="lustre lustreapi"])
    
    # Add correct -I and -L flags
    AS_IF([test -d "$with_lustre/include"],
        [check_lustre_CPPFLAGS="-I$with_lustre/include"
            $1_CPPFLAGS="$check_lustre_CPPFLAGS"
            $1_CPPFLAGS="$check_lustre_CPPFLAGS"
            CFLAGS="$CFLAGS $check_lustre_CPPFLAGS"	    
            CPPFLAGS="$CPPFLAGS $check_lustre_CPPFLAGS"	    
            WRAPPER_EXTRA_CPPFLAGS="$WRAPPER_EXTRA_CPPFLAGS $check_lustre_CPPFLAGS"
            WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $check_lustre_CPPFLAGS"], 
	[ompi_check_lustre_happy="no"])
    
    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
	[AS_IF([test -d "$with_lustre/lib"],
		[check_lustre_LDFLAGS="-L$with_lustre/lib"
		    $1_LDFLAGS="$check_lustre_LDFLAGS"
		    LDFLAGS="$LDFLAGS $check_lustre_LDFLAGS"
		    WRAPPER_EXTRA_LDFLAGS="$WRAPPER_EXTRA_LDFLAGS $check_lustre_LDFLAGS"],
		[ompi_check_lustre_happy="no"]) 
    ],[])
	    
    # Try to find all the lustre libraries
    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
	[ AS_IF([test -n "$with_lustre_libs"]
		[for lib in $with_lustre_libs ; do
		    check_lustre_LIBS="$check_lustre_LIBS -l$lib"
		    done]) 
		
	    $1_LIBS="$check_lustre_LIBS"
	    LIBS="$LIBS $check_lustre_LIBS"
	    WRAPPER_EXTRA_LIBS="$WRAPPER_EXTRA_LIBS $check_lustre_LIBS"

            # check for lustre
	    AC_CHECK_HEADERS([lustre.h],
		[AC_MSG_CHECKING([if possible to link LUSTRE])
		    AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <lustre.h>], 
				[int i;])],
			[AC_MSG_RESULT([yes])
			    ompi_check_lustre_happy="yes"],
			[AC_MSG_RESULT([no])
			    ompi_check_lustre_happy="no"])],
		[ompi_check_lustre_happy="no"])
    ])

    AS_IF([test "$ompi_check_lustre_happy" = "no"],
     [WRAPPER_EXTRA_LIBS="$check_lustre_save_wrapper_extra_libs"
	 WRAPPER_EXTRA_LDFLAGS="$check_lustre_save_wrapper_extra_ldflags"
	 WRAPPER_EXTRA_CFLAGS="$check_lustre_save_wrapper_extra_cflags"
	 WRAPPER_EXTRA_CPPFLAGS="$check_lustre_save_wrapper_extra_cppflags"
     ])

    LDFLAGS="$check_lustre_save_LDFLAGS"
    CFLAGS="$check_lustre_save_CFLAGS"
    CPPFLAGS="$check_lustre_save_CPPFLAGS"
    LIBS="$check_lustre_save_LIBS"

    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_lustre" -a "$with_lustre" != "no"],
                  [echo LUSTRE support not found])
              $3])

    with_lustre="$temp_with_lustre"
    with_lustre_libs="$temp_with_lustre_libs"
])
