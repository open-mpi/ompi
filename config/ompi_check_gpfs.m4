dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2018 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2018      University of Houston. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_GPFS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if GPFS support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_GPFS],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_gpfs_happy])

    # Get some configuration information
    AC_ARG_WITH([gpfs],
        [AS_HELP_STRING([--with-gpfs(=DIR)],
             [Build Gpfs support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

    # Add correct -I and -L flags
    OAC_CHECK_PACKAGE([gpfs],
                      [$1],
                      [gpfs.h],
                      [gpfs],
                      [gpfs_lib_init],
                      [ompi_check_gpfs_happy="yes"],
                      [ompi_check_gpfs_happy="no"])

    OPAL_SUMMARY_ADD([OMPIO File Systems], [IBM Spectrum Scale/GPFS], [], [${$1_SUMMARY}])

    AS_IF([test "$ompi_check_gpfs_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_gpfs" && test "$with_gpfs" != "no"],
                 [AC_MSG_ERROR([GPFS support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])
