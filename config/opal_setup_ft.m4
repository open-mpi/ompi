dnl
dnl Copyright (c) 2004-2020 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2012 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

#
# --with-ft=TYPE
#  TYPE:
#    - mpi (synonym for 'ulfm')
#    - LAM (synonym for 'cr' currently)
#    - cr
#  /* General FT sections */
#  #if OPAL_ENABLE_FT == 0 /* FT Disabled globaly */
#  #if OPAL_ENABLE_FT == 1 /* FT Enabled globaly */
#  /* ULFM Specific sections */
#  #if OPAL_ENABLE_FT_MPI == 0 /* FT ULFM Disabled */
#  #if OPAL_ENABLE_FT_MPI == 1 /* FT ULFM Enabled */
#

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OPAL_SETUP_FT_BANNER],[
    opal_show_subtitle "Fault tolerance"
])

AC_DEFUN([OPAL_SETUP_FT_OPTIONS],[
    # define a variable that tells us that these options were enabled
    opal_setup_ft_options="yes"
    AC_ARG_WITH([ft],
                [AS_HELP_STRING([--with-ft=TYPE],
                [Specify the type of fault tolerance to enable. Options: mpi (ULFM), (default: mpi)])],
                [],
                [with_ft=auto]) # If not specified act as if --with-ft=mpi, but make external prte support failure only if hard requested
])

AC_DEFUN([OPAL_SETUP_FT],[
    AC_REQUIRE([OPAL_SETUP_FT_BANNER])
    if test "$opal_setup_ft_options" = "yes"; then
        AC_MSG_CHECKING([if want fault tolerance])
    fi

    if test x"$with_ft" != "xno"; then
        opal_want_ft=1
        opal_want_ft_mpi=0
        opal_want_ft_type=none

        as_save_IFS=$IFS
        IFS=","
        for opt in $with_ft; do
            IFS=$as_save_IFS

            if test "$opt" = "LAM" || test "$opt" = "lam" || test "$opt" = "CR" || test "$opt" = "cr"; then
                AC_MSG_RESULT([Support for C/R FT has been removed in OMPI 5.0])
            fi
            # Default value
            if test "$opt" = "auto"; then
                opal_want_ft_mpi=1
            elif test "$opt" = "yes"; then
                opal_want_ft_mpi=1
            elif test "$opt" = "ULFM"; then
                opal_want_ft_mpi=1
            elif test "$opt" = "ulfm"; then
                opal_want_ft_mpi=1
            elif test "$opt" = "MPI"; then
                opal_want_ft_mpi=1
            elif test "$opt" = "mpi"; then
                opal_want_ft_mpi=1
            else
                AC_MSG_RESULT([Unrecognized FT TYPE: $opt])
                AC_MSG_ERROR([Cannot continue])
            fi
        done
        if test "$opal_want_ft_mpi" = 1; then
            opal_want_ft_type="mpi"
        fi

        # If we use external PRTE, does it support FT?
        AS_IF([test "$internal_prrte_build" = "0" -a "$opal_want_ft_type" != "none"], [
            AS_IF([prte_info | $GREP "Resilience support: yes"], [], [
                AS_IF([test "$with_ft" != auto], [
                    AC_MSG_ERROR([Requested enabling fault-tolerance and using external launcher, but external PRTE doesn't support resilience; you can either use the internal PRTE, recompile the external PRTE with fault-tolerance, or disable fault-tolerance. ABORTING.])
                ], [
                    AC_MSG_WARN([**************************************************])
                    AC_MSG_WARN([*** Requested external PRTE which doesn't have   *])
                    AC_MSG_WARN([*** Resilience compiled-in.                      *])
                    AC_MSG_WARN([*** To enable Open MPI Fault-Tolerance, either   *])
                    AC_MSG_WARN([***   use the internal PRTE, or                  *])
                    AC_MSG_WARN([***   compile the external PRTE with resilience  *])
                    AC_MSG_WARN([*** DISABLING FAULT TOLERANCE SUPPORT.           *])
                    AC_MSG_WARN([**************************************************])
                    opal_want_ft_mpi=0
                    opal_want_ft_type="none"
                ])
            ])
        ])
        AC_MSG_RESULT([Enabled $opal_want_ft_type (Specified $with_ft)])
        AS_IF([test "$opal_want_ft_type" != "none"], [
            AC_MSG_WARN([**************************************************])
            AC_MSG_WARN([*** Fault Tolerance Integration into Open MPI is *])
            AC_MSG_WARN([*** compiled-in, but off by default. Use mpiexec *])
            AC_MSG_WARN([*** and MCA parameters to turn it on.            *])
            AC_MSG_WARN([*** Not all components support fault tolerance.  *])
            AC_MSG_WARN([**************************************************])
        ])
    else
        opal_want_ft=0
        opal_want_ft_mpi=0
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled fault tolerance])
        fi
    fi
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT], [$opal_want_ft],
                       [Enable fault tolerance general components and logic])
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_MPI], [$opal_want_ft_mpi],
                       [Enable fault tolerance MPI ULFM components and logic])
    AM_CONDITIONAL(WANT_FT, test "$opal_want_ft" = "1")
    AM_CONDITIONAL(WANT_FT_MPI, test "$opal_want_ft_mpi" = "1")

    if test "$opal_setup_ft_options" = "yes"; then
        AC_MSG_CHECKING([if want checkpoint/restart enabled debugging option])
    fi
    if test "$opal_want_ft" = "0"; then
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled (fault tolerance disabled --without-ft)])
        fi
    else
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled])
        fi
    fi

    OPAL_SUMMARY_ADD([Miscellaneous], [Fault Tolerance support], [], [$opal_want_ft_type])
])
