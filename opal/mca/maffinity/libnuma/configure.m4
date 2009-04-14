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
# Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
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
                [AC_HELP_STRING([--with-libnuma=(DIR)],
                                [Build libnuma support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([libnuma], [$with_libnuma], [include/numa.h])
    AC_ARG_WITH([libnuma-libdir],
                [AC_HELP_STRING([--with-libnuma-libdir=DIR],
                                [Directory where the libnuma software is installed])])
    OMPI_CHECK_WITHDIR([libnuma], [$with_libnuma], [include/numa.h])

    AS_IF([test ! -z "$with_libnuma" -a "$with_libnuma" != "yes"],
          [ompi_check_libnuma_dir="$with_libnuma"])
    AS_IF([test ! -z "$with_libnuma_libdir" -a "$with_libnuma_libdir" != "yes"],
          [ompi_check_libnuma_libdir="$with_libnuma_libdir"])
    AS_IF([test "$with_libnuma" = "no"],
          [maffinity_libnuma_happy="no"],
          [maffinity_libnuma_happy="yes"])

    AS_IF([test "$maffinity_libnuma_happy" = "yes"],
          [OMPI_CHECK_PACKAGE([maffinity_libnuma],
                              [numa.h],
                              [numa],
                              [numa_available],
                              [],
                              [$ompi_check_libnuma_dir],
                              [$ompi_check_libnuma_libdir],
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

   # Check for MPOL_MF_MOVE
   AS_IF([test "$maffinity_libnuma_happy" = "yes"],
         [AC_CHECK_DECLS([MPOL_MF_MOVE])])
 
    # substitute in the things needed to build libnuma
    AC_SUBST([maffinity_libnuma_CPPFLAGS])
    AC_SUBST([maffinity_libnuma_LDFLAGS])
    AC_SUBST([maffinity_libnuma_LIBS])
])dnl
