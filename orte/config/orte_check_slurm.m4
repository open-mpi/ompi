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
    AC_ARG_WITH([slurm-pmi-libdir],
       [AC_HELP_STRING([--with-slurm-pmi-libdir=DIR],
                       [Search for SLURM PMI libraries in DIR ])])

    # Defaults
    orte_check_slurm_dir=
    orte_check_slurm_libdir=
    orte_check_slurm_dir_msg="compiler default"
    orte_check_slurm_libdir_msg="linker default"
    # Save directory names if supplied
    AS_IF([test ! -z "$with_slurm" -a "$with_slurm" != "yes"],
          [orte_check_slurm_dir="$with_slurm"
           orte_check_slurm_dir_msg="$orte_check_slurm_dir (from --with-slurm)"])
    AS_IF([test ! -z "$with_slurm_pmi_libdir" -a "$with_slurm_pmi_libdir" != "yes"],
          [orte_check_slurm_libdir="$with_slurm_pmi_libdir"
           orte_check_slurm_libdir_msg="$orte_check_slurm_pmi_libdir (from --with-slurm-pmi-libdir)"])

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

    orte_check_slurm_$1_save_CPPFLAGS="$CPPFLAGS"
    orte_check_slurm_$1_save_LDFLAGS="$LDFLAGS"
    orte_check_slurm_$1_save_LIBS="$LIBS"

   AC_MSG_CHECKING([if user requested PMI support])
   orte_enable_slurm_pmi=0
   AS_IF([test "$with_slurm_pmi" = "yes"],
         [AC_MSG_RESULT([yes])
          orte_want_pmi_support=yes
          AC_MSG_CHECKING([if SLURM PMI support installed])
          OMPI_CHECK_PACKAGE([$1],
                             [slurm/pmi.h],
                             [pmi],
                             [PMI_Init],
                             [-lpmi],
                             [$orte_check_slurm_dir],
                             [$orte_check_slurm_libdir],
                             [orte_have_pmi_support="yes"],
                             [orte_have_pmi_support="no"])
         AS_IF([test "$orte_have_pmi_support" = "yes"],
               [AC_MSG_RESULT([yes])
                AC_MSG_WARN([SLURM PMI SUPPORT HAS BEEN INCLUDED - RESULTING])
                AC_MSG_WARN([BINARIES ARE SUBJECT TO ADDITIONAL LICENSING])
                AC_MSG_WARN([RESTRICTIONS - SEE THE SLURM LICENSE FOR INFO])
                orte_enable_slurm_pmi=1],
               [AC_MSG_RESULT([no])
                AC_MSG_WARN([SLURM PMI support requested (via --with-slurm-pmi) but not found.])
                AC_MSG_ERROR([Aborting.])])],
         [AC_MSG_RESULT([no])
          orte_want_pmi_support=no])
   AC_DEFINE_UNQUOTED([WANT_SLURM_PMI_SUPPORT],
                      [$orte_enable_slurm_pmi],
                      [Whether we want SLURM PMI support])

    CPPFLAGS="$orte_check_slurm_$1_save_CPPFLAGS"
    LDFLAGS="$orte_check_slurm_$1_save_LDFLAGS"
    LIBS="$orte_check_slurm_$1_save_LIBS"

    # Reset for the next time we're called
    orte_check_slurm_dir=

    AS_IF([test "$orte_check_slurm_happy" = "yes"], 
          [$2], 
          [$3])
])
