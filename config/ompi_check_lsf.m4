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
# Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# check for lsf
# OMPI_CHECK_LSF(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_LSF],[
    AC_ARG_WITH([lsf],
        [AC_HELP_STRING([--with-lsf(=DIR)],
             [Build LSF support])])
    OMPI_CHECK_WITHDIR([lsf], [$with_lsf], [include/lsf/lsbatch.h])
    AC_ARG_WITH([lsf-libdir],
       [AC_HELP_STRING([--with-lsf-libdir=DIR],
             [Search for LSF libraries in DIR])])
    OMPI_CHECK_WITHDIR([lsf-libdir], [$with_lsf_libdir], [libbat.*])

    # Defaults
    ompi_check_lsf_dir_msg="compiler default"
    ompi_check_lsf_libdir_msg="linker default"

    # Save directory names if supplied
    AS_IF([test ! -z "$with_lsf" -a "$with_lsf" != "yes"],
          [ompi_check_lsf_dir="$with_lsf"
           ompi_check_lsf_dir_msg="$ompi_check_lsf_dir (from --with-lsf)"])
    AS_IF([test ! -z "$with_lsf_libdir" -a "$with_lsf_libdir" != "yes"],
          [ompi_check_lsf_libdir="$with_lsf_libdir"
           ompi_check_lsf_libdir_msg="$ompi_check_lsf_libdir (from --with-lsf-libdir)"])

    # If no directories were specified, look for LSF_LIBDIR,
    # LSF_INCLUDEDIR, and/or LSF_ENVDIR.
    AS_IF([test -z "$ompi_check_lsf_dir" -a -z "$ompi_check_lsf_libdir"],
          [AS_IF([test ! -z "$LSF_ENVDIR" -a -z "$LSF_LIBDIR" -a -f "$LSF_ENVDIR/lsf.conf"],
                 [LSF_LIBDIR=`egrep ^LSF_LIBDIR= $LSF_ENVDIR/lsf.conf | cut -d= -f2-`])
           AS_IF([test ! -z "$LSF_ENVDIR" -a -z "$LSF_INCLUDEDIR" -a -f "$LSF_ENVDIR/lsf.conf"],
                 [LSF_INCLUDEDIR=`egrep ^LSF_INCLUDEDIR= $LSF_ENVDIR/lsf.conf | cut -d= -f2-`])
           AS_IF([test ! -z "$LSF_LIBDIR"],
                 [ompi_check_lsf_libdir=$LSF_LIBDIR
                  ompi_check_lsf_libdir_msg="$LSF_LIBDIR (from \$LSF_LIBDIR)"])
           AS_IF([test ! -z "$LSF_INCLUDEDIR"],
                 [ompi_check_lsf_dir=`dirname $LSF_INCLUDEDIR`
                  ompi_check_lsf_dir_msg="$ompi_check_lsf_dir (from \$LSF_INCLUDEDIR)"])])

    AS_IF([test "$with_lsf" = "no"],
          [ompi_check_lsf_happy="no"],
          [ompi_check_lsf_happy="yes"])

    ompi_check_lsf_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_lsf_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_lsf_$1_save_LIBS="$LIBS"

    AS_IF([test "$ompi_check_lsf_happy" = "yes"], 
          [AC_MSG_CHECKING([for LSF dir])
           AC_MSG_RESULT([$ompi_check_lsf_dir_msg])
           AC_MSG_CHECKING([for LSF library dir])
           AC_MSG_RESULT([$ompi_check_lsf_libdir_msg])
           OMPI_CHECK_PACKAGE([$1],
                              [lsf/lsbatch.h],
                              [bat],
                              [lsb_launch],
                              [-llsf],
                              [$ompi_check_lsf_dir],
                              [$ompi_check_lsf_libdir],
                              [ompi_check_lsf_happy="yes"],
                              [ompi_check_lsf_happy="no"])])

    CPPFLAGS="$ompi_check_lsf_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_lsf_$1_save_LDFLAGS"
    LIBS="$ompi_check_lsf_$1_save_LIBS"

    # Reset for the next time we're called
    ompi_check_lsf_dir=
    ompi_check_lsf_libdir=

    AS_IF([test "$ompi_check_lsf_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_lsf" -a "$with_lsf" != "no"],
                 [AC_MSG_WARN([LSF support requested (via --with-lsf) but not found.])
                  AC_MSG_ERROR([Aborting.])])
           $3])
])
