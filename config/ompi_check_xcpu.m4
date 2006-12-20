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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_XCPU(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_XCPU],[
    AC_ARG_WITH([xcpu],
                [AC_HELP_STRING([--with-xcpu],
                                [=yes will Build XCPU launcher component (default: no)])])

    AS_IF([test ! -z "$with_xcpu" -a "$with_xcpu" = "no"],[$3], [ 
        ompi_check_xcpu_save_CPPFLAGS="$CPPFLAGS"
        ompi_check_xcpu_save_LDFLAGS="$LDFLAGS"
        ompi_check_xcpu_save_LIBS="$LIBS"

	AS_IF([test ! -z "$with_xcpu" -a "$with_xcpu" != "yes"], 
	      [CPPFLAGS="$CPPFLAGS -I$with_xcpu/include"
               LDFLAGS="$LDFLAGS -L$with_xcpu/lib"])

        AC_CHECK_HEADERS([libxcpu.h],
                         [AC_CHECK_LIB([xcpu], 
                                       [xp_command_create],
                                       [ompi_check_xcpu_happy="yes"],
                                       [ompi_check_xcpu_happy="no"],
				       [-lstrutil -lspclient -lspfs -lelf])],
                         [ompi_check_xcpu_happy="no"],
			 [#include <stdio.h>
			  #include <spfs.h>
			  #include <spclient.h>
			  #include <strutil.h>])

        CPPFLAGS="$ompi_check_xcpu_save_CPPFLAGS"
        LDFLAGS="$ompi_check_xcpu_save_LDFLAGS"
        LIBS="$ompi_check_xcpu_save_LIBS"

        AS_IF([test "$ompi_check_xcpu_happy" != "no"], 
              [AS_IF([test ! -z "$with_xcpu" -a "$with_xcpu" != "yes"], 
                     [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_xcpu/include"
                      $1_LDFLAGS="$$1_LDFLAGS -L$with_xcpu/lib"])
               $1_LIBS="$$1_LIBS -lxcpu -lstrutil -lspclient -lspfs -lelf" $2], 
               [AS_IF([test ! -z "$with_xcpu"],
                     [AC_MSG_ERROR([Xcpu support request but not found.  Perhaps
you need to specify the location of the Xcpu libraries.])])
               $3])
    ])
])
