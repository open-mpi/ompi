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
# Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_XGRID(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_XGRID],[
  AC_REQUIRE([AC_PROG_OBJC])

  AC_ARG_WITH([xgrid],
    [AC_HELP_STRING([--with-xgrid],
    [Build support for the Apple Xgrid batch system (default: yes)])])

  AS_IF([test "$with_xgrid" != "no"],
    [OMPI_LANG_LINK_WITH_C([Objective C],
       [AC_CACHE_CHECK([for XGridFoundation Framework],
          [ompi_cv_check_xgrid_foundation],
          [_OMPI_CHECK_XGRID([ompi_cv_check_xgrid_foundation="yes"],
             [ompi_cv_check_xgrid_foundation="no"])])
        AS_IF([test "$ompi_cv_check_xgrid_foundation" = "yes"],
          [ompi_check_xgrid_happy="yes"],
          [ompi_check_xgrid_happy="no"])],
       [ompi_check_xgrid_happy="no"])],
    [ompi_check_xgrid_happy="no"])

    AS_IF([test "$ompi_check_xgrid_happy" = "yes"], 
          [$1_LDFLAGS="$$1_LDFLAGS -framework XGridFoundation -framework Foundation"
           $2], [$3])
])


# _OMPI_CHECK_XGRID([action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([_OMPI_CHECK_XGRID],[
  AC_LANG_PUSH(Objective C)
  ompi_check_xgrid_save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS -framework XGridFoundation -framework Foundation"
  AC_TRY_LINK([#import <Foundation/Foundation.h>
#import <XgridFoundation/XgridFoundation.h>
#import <Foundation/NSString.h>
],
    [NSLog(@"%@", XGConnectionKeyIsOpened);],
    [$1], [$2])
  LDFLAGS="$ompi_check_xgrid_save_LDFLAGS"
  AC_LANG_POP(Objective C)
])
