# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2013 The University of Tennessee and The University
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

AC_DEFUN([MCA_opal_mpool_udreg_CONFIG],[
    AC_CONFIG_FILES([opal/mca/mpool/udreg/Makefile])

    AC_ARG_WITH([udreg], [AC_HELP_STRING([--with-udreg],
		[Build support for Cray udreg support. Set PKG_CONFIG_PATH env. variable to specify alternate path.])])

    mpool_udreg_happy="no"

    AS_IF([test "$with_udreg" = "no"],
          [mpool_udreg_happy="no"],
          [PKG_CHECK_MODULES([CRAY_UDREG], [cray-udreg],
                      [mpool_udreg_LDFLAGS="$CRAY_UDREG_LIBS"
                       mpool_udreg_CPPFLAGS="$CRAY_UDREG_CFLAGS"
                       mpool_udreg_happy="yes"],
                      [AC_MSG_RESULT([no])
                       mpool_udreg_happ="no"])])

    AS_IF([test "$mpool_udreg_happy" = "yes"], [$1], [$2])


    # substitute in the things needed to build udreg/mpool
    AC_SUBST([mpool_udreg_CPPFLAGS])
    AC_SUBST([mpool_udreg_LDFLAGS])
    AC_SUBST([mpool_udreg_LIBS])
])dnl
