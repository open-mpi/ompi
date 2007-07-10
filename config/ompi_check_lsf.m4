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

# check for lsf
# OMPI_CHECK_LSF(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_LSF],[
    AC_ARG_WITH([lsf],
                [AC_HELP_STRING([--with-lsf],
                                [Directory where the LSF software is installed])])

    ompi_check_lsf_found=no
    AS_IF([test "$with_lsf" = "no"],
          [ompi_check_lsf_happy="no"],
          [ompi_check_lsf_happy="yes"
           AS_IF([test ! -z "$with_lsf" -a "$with_lsf" != "yes"],
                 [ompi_check_lsf_dir="$with_lsf"],
                 [ompi_check_lsf_dir=""])])

])
