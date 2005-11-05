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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# MCA_btl_mvapi_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_mvapi_CONFIG],[
    OMPI_CHECK_MVAPI([btl_mvapi],
                     [btl_mvapi_happy="yes"],
                     [btl_mvapi_happy="no"])

    AS_IF([test "$btl_mvapi_happy" = "yes"],
          [btl_mvapi_WRAPPER_EXTRA_LDFLAGS="$btl_mvapi_LDFLAGS"
           btl_mvapi_WRAPPER_EXTRA_LIBS="$btl_mvapi_LIBS"
           $1],
          [$2])

    # Many of the vapi.h files floating around don't obey ISO99 C
    # standard, so cause oodles of warnings with -pedantic and
    # -Wundef.  Remove them from CFLAGS, which is then used to
    # forcefully override CFLAGS in the makefile for MVAPI
    # components
    btl_mvapi_CFLAGS="`echo $CFLAGS | sed 's/-pedantic//g'`"
    btl_mvapi_CFLAGS="`echo $btl_mvapi_CFLAGS | sed 's/-Wundef//g'`"
    AS_IF([test "$btl_mvapi_CFLAGS" != "$CFLAGS" -a "$btl_mvapi_happy" = "yes"],
          [AC_MSG_WARN([Removed -pedantic and -Wundef from CFLAGS for
mvapi component because some vapi.h files are not really ANSI C])])

    # substitute in the things needed to build mvapi
    AC_SUBST([btl_mvapi_CFLAGS])
    AC_SUBST([btl_mvapi_CPPFLAGS])
    AC_SUBST([btl_mvapi_LDFLAGS])
    AC_SUBST([btl_mvapi_LIBS])
])dnl
