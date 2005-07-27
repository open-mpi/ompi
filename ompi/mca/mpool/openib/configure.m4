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


# MCA_mpool_openib_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_mpool_openib_CONFIG],[
    OMPI_CHECK_OPENIB([mpool_openib],
                     [mpool_openib_happy="yes"],
                     [mpool_openib_happy="no"])

    AS_IF([test "$mpool_openib_happy" = "yes"],
          [mpool_openib_WRAPPER_EXTRA_LDFLAGS="$mpool_openib_LDFLAGS"
           mpool_openib_WRAPPER_EXTRA_LIBS="$mpool_openib_LIBS"
           $1],
          [$2])

    # Many of the vapi.h files floating around don't obey ISO99 C
    # standard, so cause oodles of warnings with -pedantic and
    # -Wundef.  Remove them from CFLAGS, which is then used to
    # forcefully override CFLAGS in the makefile for OPENIB
    # components
    mpool_openib_CFLAGS="`echo $CFLAGS | sed 's/-pedantic//g'`"
    mpool_openib_CFLAGS="`echo $mpool_openib_CFLAGS | sed 's/-Wundef//g'`"
    AS_IF([test "$mpool_openib_CFLAGS" != "$CFLAGS"],
          [AC_MSG_WARN([Removed -pedantic and -Wundef from CFLAGS for
openib component because some vapi.h files are not really ANSI C])])

    # substitute in the things needed to build openib
    AC_SUBST([mpool_openib_CFLAGS])
    AC_SUBST([mpool_openib_CPPFLAGS])
    AC_SUBST([mpool_openib_LDFLAGS])
    AC_SUBST([mpool_openib_LIBS])
])dnl