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
            AC_MSG_RESULT([Enabled $opal_want_ft_type (Specified $with_ft)])
            AC_MSG_WARN([****************************************************])
            AC_MSG_WARN([*** The checkpoint/restart integration in Open MPI *])
            AC_MSG_WARN([*** has been removed.                              *])
            AC_MSG_WARN([****************************************************])
            AC_MSG_ERROR([Cannot continue])
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
    AM_CONDITIONAL(WANT_FT, test "$opal_want_ft" = "1")

    if test "$opal_setup_ft_options" = "yes"; then
        AC_MSG_CHECKING([if want checkpoint/restart enabled debugging option])
    fi
])
