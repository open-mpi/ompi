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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_SLURM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_SLURM],[
    AC_ARG_WITH([slurm],
                [AC_HELP_STRING([--with-slurm],
                                [Build SLURM scheduler component (default: yes)])])
    AC_ARG_WITH([slurm-pmi],
                [AC_HELP_STRING([--with-slurm-pmi],
                                [Build SLURM PMI support (default: no)])])

    if test "$with_slurm" = "no" ; then
        orte_check_slurm_happy="no"
    elif test "$with_slurm" = "" ; then
        # unless user asked, only build slurm component on linux, AIX,
        # and OS X systems (these are the platforms that SLURM
        # supports)
        case $host in
            *-linux*|*-aix*|*-apple-darwin*)
                orte_check_slurm_happy="yes"
                ;;
            *)
                AC_MSG_CHECKING([for SLURM srun in PATH])
                OPAL_WHICH([srun], [ORTE_CHECK_SLURM_SRUN])
                if test "$ORTE_CHECK_SLURM_SRUN" = ""; then
                    orte_check_slurm_happy="no"
                else
                    orte_check_slurm_happy="yes"
                fi
                AC_MSG_RESULT([$orte_check_slurm_happy])
                ;;
        esac
    else 
        orte_check_slurm_happy="yes"
    fi

    AS_IF([test "$orte_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([fork],
                         [orte_check_slurm_happy="yes"],
                         [orte_check_slurm_happy="no"])])

    AS_IF([test "$orte_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([execve],
                         [orte_check_slurm_happy="yes"],
                         [orte_check_slurm_happy="no"])])

    AS_IF([test "$orte_check_slurm_happy" = "yes"],
          [AC_CHECK_FUNC([setpgid],
                         [orte_check_slurm_happy="yes"],
                         [orte_check_slurm_happy="no"])])

   AC_MSG_CHECKING([if user requested PMI support])
   orte_enable_slurm_pmi=0
   AS_IF([test "$with_slurm_pmi" = "yes"],
         [AC_MSG_RESULT([yes])
          orte_want_pmi_support=yes
          AC_MSG_CHECKING([if SLURM PMI support installed])
          AC_CHECK_HEADER([slurm/pmi.h], [orte_have_pmi_support=yes], [orte_have_pmi_support=no])]
         AS_IF([test "$orte_have_pmi_support" = "yes"],
               [AC_MSG_RESULT([yes])
                AC_MSG_WARN([SLURM PMI SUPPORT HAS BEEN INCLUDED - RESULTING])
                AC_MSG_WARN([BINARIES ARE SUBJECT TO ADDITIONAL LICENSING])
                AC_MSG_WARN([RESTRICTIONS - SEE THE SLURM LICENSE FOR INFO])
                orte_enable_slurm_pmi=1],
               [AC_MSG_RESULT([no])
                AC_MSG_WARN([SLURM PMI support requested (via --with-slurm-pmi) but not found.])
                AC_MSG_ERROR([Aborting.])]),
         [AC_MSG_RESULT([no])
          orte_want_pmi_support=no])
   AC_DEFINE_UNQUOTED([WANT_SLURM_PMI_SUPPORT],
                      [$orte_enable_slurm_pmi],
                      [Whether we want SLURM PMI support])

    AS_IF([test "$orte_check_slurm_happy" = "yes"], 
          [$2], 
          [$3])
])
