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


# OMPI_CHECK_SLURM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_SLURM],[
    AC_ARG_WITH([slurm],
                [AC_HELP_STRING([--with-slurm],
                                [Build SLURM scheduler component (default: yes)])])

    if test "$with_slurm" = "no" ; then
        ompi_check_slurm_happy="no"
    elif test "$with_slurm" = "" ; then
        # unless user asked, only build slurm component on
        # linux systems.
        case $host in
            *-linux*|*-aix*)
                ompi_check_slurm_happy="yes"
                ;;
            *)
                ompi_check_slurm_happy="no"
                ;;
        esac
    else 
        ompi_check_slurm_happy="yes"
    fi

    AS_IF([test "$ompi_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([fork],
                         [ompi_check_slurm_happy="yes"],
                         [ompi_check_slurm_happy="no"])])

    AS_IF([test "$ompi_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([execve],
                         [ompi_check_slurm_happy="yes"],
                         [ompi_check_slurm_happy="no"])])

    AS_IF([test "$ompi_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([setpgid],
                         [ompi_check_slurm_happy="yes"],
                         [ompi_check_slurm_happy="no"])])

    AS_IF([test "$ompi_check_slurm_happy" = "yes"], 
          [$2], 
          [$3])
])
