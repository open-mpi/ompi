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


# MCA_<framework>_<component>_CONFIG([action-if-can-compile], 
#                                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_maffinity_libnuma_CONFIG],[
    AC_ARG_WITH([libnuma],
                [AC_HELP_STRING([--with-libnuma],
                                [Directory where the libnuma software is installed])])

    AS_IF([test "$with_libnuma" = "no"],
          [maffinity_libnuma_happy="no"],
          [maffinity_libnuma_happy="yes"])

    AS_IF([test "$maffinity_libnuma_happy" = "yes"],
          [OMPI_CHECK_PACKAGE([maffinity_libnuma],
                              [numa.h],
                              [numa],
                              [numa_available],
                              [],
                              [$with_libnuma],
                              [],
                              [maffinity_libnuma_happy="yes"],
                              [maffinity_libnuma_happy="no"])])

    AS_IF([test "$maffinity_libnuma_happy" = "yes"],
          [maffinity_libnuma_WRAPPER_EXTRA_LDFLAGS="$maffinity_libnuma_LDFLAGS"
           maffinity_libnuma_WRAPPER_EXTRA_LIBS="$maffinity_libnuma_LIBS"
           $1],
          [$2])

    # sanity check
    AS_IF([test "$maffinity_libnuma_happy" = "no"],
          [AS_IF([test "$with_libnuma" != "no" -a ! -z "$with_libnuma"],
                 [AC_MSG_ERROR([maffinity:libnuma requested but not found.  Aborting])])])

    # substitute in the things needed to build gm
    AC_SUBST([maffinity_libnuma_CPPFLAGS])
    AC_SUBST([maffinity_libnuma_LDFLAGS])
    AC_SUBST([maffinity_libnuma_LIBS])
])dnl
