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


# MCA_btl_mvapi_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
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

    # Many vapi.h's have horrid semantics and don't obey ISOC99
    # standards.  So we have to turn off flags like -pedantic.  Sigh.
    btl_mvapi_CFLAGS="`echo $CFLAGS | sed 's/-pedantic//g'`"
    btl_mvapi_CFLAGS="`echo $btl_mvapi_CFLAGS | sed 's/-Wundef//g'`"

    # substitute in the things needed to build Portals
    AC_SUBST([btl_mvapi_CFLAGS])
    AC_SUBST([btl_mvapi_CPPFLAGS])
    AC_SUBST([btl_mvapi_LDFLAGS])
    AC_SUBST([btl_mvapi_LIBS])
])dnl
