dnl
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
#    - LAM (synonym for 'cr' currently)
#    - cr
#  /* General FT sections */
#  #if OPAL_ENABLE_FT == 0 /* FT Disabled globaly */
#  #if OPAL_ENABLE_FT == 1 /* FT Enabled globaly */
#  /* CR Specific sections */
#  #if OPAL_ENABLE_FT_CR == 0 /* FT Ckpt/Restart Disabled */
#  #if OPAL_ENABLE_FT_CR == 1 /* FT Ckpt/Restart Enabled */
#

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OPAL_SETUP_FT_BANNER],[
    opal_show_subtitle "Fault tolerance" 
])

AC_DEFUN([OPAL_SETUP_FT_OPTIONS],[
    AC_REQUIRE([OPAL_SETUP_FT_BANNER])
    # define a variable that tells us that these options were enabled
    opal_setup_ft_options="yes"
    AC_ARG_WITH(ft,
                [AC_HELP_STRING([--with-ft=TYPE],
                [Specify the type of fault tolerance to enable. Options: LAM (LAM/MPI-like), cr (Checkpoint/Restart), (default: disabled)])],
                [opal_want_ft=1],
                [opal_want_ft=0])

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
    if test "$opal_setup_ft_options" = "yes"; then 
        AC_MSG_CHECKING([if want fault tolerance])
    fi
    if test "x$with_ft" != "x" || test "$opal_want_ft" = "1"; then
        opal_want_ft=1
        opal_want_ft_cr=0
        opal_want_ft_type=none

        as_save_IFS=$IFS
        IFS=","
        for opt in $with_ft; do
            IFS=$as_save_IFS

            # Default value
            if test "$opt" = "" || test "$opt" = "yes"; then
                opal_want_ft_cr=1
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
        if test "$opal_want_ft_cr" = 1; then
            opal_want_ft_type="cr"
        fi

        AC_MSG_RESULT([Enabled $opal_want_ft_type (Specified $with_ft)])
        AC_MSG_WARN([**************************************************])
        AC_MSG_WARN([*** Fault Tolerance Integration into Open MPI is *])
        AC_MSG_WARN([*** a research quality implementation, and care  *])
        AC_MSG_WARN([*** should be used when choosing to enable it.   *])
        AC_MSG_WARN([**************************************************])
    else
        opal_want_ft=0
        opal_want_ft_cr=0
        if test "$opal_setup_ft_options" = "yes"; then 
            AC_MSG_RESULT([Disabled fault tolerance])
        fi
    fi
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT], [$opal_want_ft],
                       [Enable fault tolerance general components and logic])
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_CR], [$opal_want_ft_cr],
                       [Enable fault tolerance checkpoint/restart components and logic])
    AM_CONDITIONAL(WANT_FT, test "$opal_want_ft" = "1")
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
])
