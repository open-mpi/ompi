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
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_mpool_udreg_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mpool/udreg/Makefile])

    AC_ARG_WITH([udreg], [AC_HELP_STRING([--with-udreg(=DIR)],
		[Build support for Cray udreg support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([udreg], [$with_udreg], [.])

    mpool_udreg_happy="no"

    if test "$with_udreg" != "no" ; then
	if test -n "$with_udreg" -a "$with_udreg" != "yes" ; then
	    ompi_check_udreg_dir="$with_udreg"
	else
	    ompi_check_udreg_dir=""
	fi

	OMPI_CHECK_PACKAGE([mpool_udreg], [udreg_pub.h], [udreg], [UDREG_CacheCreate],
	                   [], [$ompi_check_udreg_dir], ["$ompi_check_udreg_dir/lib64"],
			   [mpool_udreg_happy="yes"], [mpool_udreg_happy="no"])
    fi

    AS_IF([test "$mpool_udreg_happy" = "yes"], [$1], [$2])

    # substitute in the things needed to build ugni
    AC_SUBST([mpool_udreg_CPPFLAGS])
    AC_SUBST([mpool_udreg_LDFLAGS])
    AC_SUBST([mpool_udreg_LIBS])
])dnl
