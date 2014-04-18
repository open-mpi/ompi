# -*- Mode: shell-script ; indent-tabs-mode:nil -*-
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
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_UGNI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if GNI support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
#
# NOTES
# on Cray XE6 systems, the GNI development header (gni_pub.h) is in a
# completely different place than the ugni library (libugni).
#
# EXAMPLE CONFIGURE USAGE:
# --with-ugni=/base/path/to/libugni --with-ugni-includedir=/path/to/gni_pub.h
#
# --with-ugni=/opt/cray/ugni/default --with-ugni-includedir=/opt/cray/gni-headers/default/include

AC_DEFUN([OMPI_CHECK_UGNI], [
    if test -z "$ompi_check_ugni_happy" ; then
	OPAL_VAR_SCOPE_PUSH([ompi_check_ugni_dir ompi_check_ugni_libdir])
	ompi_check_ugni_happy="no"

	AC_ARG_WITH([ugni], [AC_HELP_STRING([--with-ugni(=DIR)],
		    [Build uGNI support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

	dnl does the path exist?
	OMPI_CHECK_WITHDIR([ugni], [$with_ugni], [.])

	AC_ARG_WITH([ugni-libdir], [AC_HELP_STRING([--with-ugni-libdir=DIR],
		    [Search for uGNI libraries in DIR])])
	OMPI_CHECK_WITHDIR([ugni-libdir], [$with_ugni_libdir], [libugni.*])

	AC_ARG_WITH([ugni-includedir],
	    [AC_HELP_STRING([--with-ugni-includedir=DIR], [Search for uGNI headers in DIR])])
	OMPI_CHECK_WITHDIR([ugni-includedir], [$with_ugni_includedir], [gni_pub.h])

	AS_IF([test "$with_ugni_includedir" != "" -a "$with_ugni_includedir" != "yes" -a "$with_ugni_includedir" != "no"],
	    [ompi_check_ugni_CPPFLAGS="-I$with_ugni_includedir"])

	if test "$with_ugni" != "no" ; then
	    if test -n "$with_ugni" -a "$with_ugni" != "yes" ; then
		ompi_check_ugni_dir="$with_ugni"
	    fi
	    if test -n "$with_ugni_libdir" -a "$with_ugni_libdir" != "yes" ; then
		ompi_check_ugni_libdir="$with_ugni_libdir"
	    fi

	    ompi_check_ugni_$1_save_CPPFLAGS="$CPPFLAGS"
	    ompi_check_ugni_$1_save_LDFLAGS="$LDFLAGS"
	    ompi_check_ugni_$1_save_LIBS="$LIBS"

	    CPPFLAGS="$CPPFLAGS $ompi_check_ugni_CPPFLAGS"

            OMPI_CHECK_PACKAGE([ompi_check_ugni], [ugni.h], [ugni], [GNI_CdmCreate],
		[], [$ompi_check_ugni_dir], [$ompi_check_ugni_libdir], [ompi_check_ugni_happy="yes"], [])

	    CPPFLAGS="$ompi_check_ugni_$1_save_CPPFLAGS"
	    LDFLAGS="$ompi_check_ugni_$1_save_LDFLAGS"
	    LIBS="$ompi_check_ugni_$1_save_LIBS"

	    if test -n "$with_ugni" -a "$ompi_check_ugni_happy" = "no" ; then
		AC_MSG_ERROR([GNI support requested but not found.  Cannot continue.])
	    fi
	fi
	OPAL_VAR_SCOPE_POP
    fi

    if test "$ompi_check_ugni_happy" = "yes" ; then
	$1_CPPFLAGS="$$1_CPPFLAGS $ompi_check_ugni_CPPFLAGS"
	$1_LDFLAGS="$$1_LDFLAGS $ompi_check_ugni_LDFLAGS"
	$1_LIBS="$$1_LIBS $ompi_check_ugni_LIBS"
	$2
    else
	$3
    fi
])
