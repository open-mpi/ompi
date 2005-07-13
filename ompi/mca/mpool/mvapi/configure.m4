# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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


# MCA_mpool_mvapi_CONFIG([action-if-can-compile], 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_mpool_mvapi_CONFIG],[
    OMPI_CHECK_MVAPI([mpool_mvapi],
                     [mpool_mvapi_happy="yes"],
                     [mpool_mvapi_happy="no"])

    AS_IF([test "$mpool_mvapi_happy" = "yes"],
          [mpool_mvapi_WRAPPER_EXTRA_LDFLAGS="$mpool_mvapi_LDFLAGS"
           mpool_mvapi_WRAPPER_EXTRA_LIBS="$mpool_mvapi_LIBS"
           $1],
          [$2])

    # Many of the vapi.h files floating around don't obey ISO99 C
    # standard, so cause oodles of warnings with -pedantic and
    # -Wundef.  Remove them from CFLAGS, which is then used to
    # forcefully override CFLAGS in the makefile for MVAPI
    # components
    mpool_mvapi_CFLAGS="`echo $CFLAGS | sed 's/-pedantic//g'`"
    mpool_mvapi_CFLAGS="`echo $mpool_mvapi_CFLAGS | sed 's/-Wundef//g'`"

    # substitute in the things needed to build Portals
    AC_SUBST([mpool_mvapi_CFLAGS])
    AC_SUBST([mpool_mvapi_CPPFLAGS])
    AC_SUBST([mpool_mvapi_LDFLAGS])
    AC_SUBST([mpool_mvapi_LIBS])
])dnl
