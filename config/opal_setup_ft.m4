dnl
dnl Copyright (c) 2004-2020 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2012 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
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
#  /* CR Specific sections */
#  #if OPAL_ENABLE_FT_CR == 0 /* FT Ckpt/Restart Disabled */
#  #if OPAL_ENABLE_FT_CR == 1 /* FT Ckpt/Restart Enabled */
#

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OPAL_SETUP_FT_BANNER],[
    opal_show_subtitle "Fault tolerance"
])

AC_DEFUN([OPAL_SETUP_FT_OPTIONS],[
    # define a variable that tells us that these options were enabled
    opal_setup_ft_options="yes"
    AC_ARG_WITH(ft,
                [AC_HELP_STRING([--with-ft=TYPE],
                [Specify the type of fault tolerance to enable. Options: mpi (ULFM), LAM (LAM/MPI-like), cr (Checkpoint/Restart) (default: mpi)])],
                [],
                [with_ft=auto]) # If not specified act as if --with-ft=mpi, but make external prte support failure only if hard requested

    #
    # Checkpoint/restart enabled debugging
    #
    AC_ARG_ENABLE([crdebug],
                  [AC_HELP_STRING([--enable-crdebug],
                  [enable checkpoint/restart debugging functionality (default: disabled)])])

    #
    # Fault Tolerance Thread
    #
    # --enable-ft-thread
    #  #if OPAL_ENABLE_FT_THREAD == 0 /* Disabled */
    #  #if OPAL_ENABLE_FT_THREAD == 1 /* Enabled  */
    #
    AC_ARG_ENABLE([ft_thread],
                  [AC_HELP_STRING([--disable-ft-thread],
                  [Disable fault tolerance thread running inside all processes. Requires OPAL thread support (default: enabled)])],
                  [enable_ft_thread="$enableval"],
                  [enable_ft_thread="undef"])

])

AC_DEFUN([OPAL_SETUP_FT],[
    AC_REQUIRE([OPAL_SETUP_FT_BANNER])
    if test "$opal_setup_ft_options" = "yes"; then
        AC_MSG_CHECKING([if want fault tolerance])
    fi

    if test x"$with_ft" != "xno"; then
        opal_want_ft=1
        opal_want_ft_cr=0
        opal_want_ft_mpi=0
        opal_want_ft_type=none

        as_save_IFS=$IFS
        IFS=","
        for opt in $with_ft; do
            IFS=$as_save_IFS

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
            elif test "$opt" = "LAM"; then
                opal_want_ft_cr=1
            elif test "$opt" = "lam"; then
                opal_want_ft_cr=1
            elif test "$opt" = "CR"; then
                opal_want_ft_cr=1
            elif test "$opt" = "cr"; then
                opal_want_ft_cr=1
            else
                AC_MSG_RESULT([Unrecognized FT TYPE: $opt])
                AC_MSG_ERROR([Cannot continue])
            fi
        done
        if test "$opal_want_ft_mpi" = 1; then
            opal_want_ft_type="mpi"
        elif test "$opal_want_ft_cr" = 1; then
            opal_want_ft_type="cr"
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
                    opal_want_ft_cr=0
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
        opal_want_ft_cr=0
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled fault tolerance])
        fi
    fi
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT], [$opal_want_ft],
                       [Enable fault tolerance general components and logic])
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_MPI], [$opal_want_ft_mpi],
                       [Enable fault tolerance MPI ULFM components and logic])
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_CR], [$opal_want_ft_cr],
                       [Enable fault tolerance checkpoint/restart components and logic])
    AM_CONDITIONAL(WANT_FT, test "$opal_want_ft" = "1")
    AM_CONDITIONAL(WANT_FT_MPI, test "$opal_want_ft_mpi" = "1")
    AM_CONDITIONAL(WANT_FT_CR,  test "$opal_want_ft_cr" = "1")

    if test "$opal_setup_ft_options" = "yes"; then
        AC_MSG_CHECKING([if want checkpoint/restart enabled debugging option])
    fi
    if test "$opal_want_ft" = "0"; then
        opal_want_prd=0
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled (fault tolerance disabled --without-ft)])
        fi
    elif test "$enable_crdebug" = "yes"; then
        opal_want_prd=1
        AC_MSG_RESULT([Enabled])
    else
        opal_want_prd=0
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled])
        fi
    fi
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_CRDEBUG], [$opal_want_prd],
                       [Whether we want checkpoint/restart enabled debugging functionality or not])

    if test "$opal_setup_ft_options" = "yes"; then
        AC_MSG_CHECKING([if want fault tolerance thread])
    fi
    # if they do not want FT support, then they do not want this thread either
    if test "$opal_want_ft" = "0"; then
        opal_want_ft_thread=0
        if test "$opal_setup_ft_options" = "yes"; then
            AC_MSG_RESULT([Disabled (fault tolerance disabled --without-ft)])
        fi
    # if --disable-ft-thread
    elif test "$enable_ft_thread" = "no"; then
        opal_want_ft_thread=0
        AC_MSG_RESULT([Disabled])
    # if default, and no progress or MPI threads
    elif test "$enable_ft_thread" = "undef" && test "$enable_opal_multi_threads" = "no" ; then
        opal_want_ft_thread=0
        AC_MSG_RESULT([Disabled (OPAL Thread Support Disabled)])
    # if default, and MPI threads enabled for C/R only
    elif test "$opal_want_ft_cr" = 1; then
        # Default: Enable
        # Make sure we have OPAL Threads enabled
        if test "$enable_opal_multi_threads" = "no"; then
            AC_MSG_RESULT([Must enable OPAL basic thread support to use this option])
            AC_MSG_ERROR([Cannot continue])
        else
            AC_MSG_RESULT([yes])
            opal_want_ft_thread=1
            AC_MSG_WARN([**************************************************])
            AC_MSG_WARN([*** Fault Tolerance with a thread in Open MPI    *])
            AC_MSG_WARN([*** is an experimental, research quality option. *])
            AC_MSG_WARN([*** It requires OPAL thread support and care     *])
            AC_MSG_WARN([*** should be used when enabling these options.  *])
            AC_MSG_WARN([**************************************************])
        fi
    # Otherwise disabled
    else
        opal_want_ft_thread=0
        AC_MSG_RESULT([Disabled (Non-C/R Fault Tolerance enabled)])
    fi
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_THREAD], [$opal_want_ft_thread],
                       [Enable fault tolerance thread in Open PAL])
    AM_CONDITIONAL(WANT_FT_THREAD, test "$opal_want_ft_thread" = "1")
    OPAL_SUMMARY_ADD([[Miscellaneous]],[[Fault Tolerance support]],[unnecessary], [$opal_want_ft_type])
])
