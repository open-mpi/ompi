dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2019 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2009 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Check to see if specific CFLAGS work
dnl $1: compiler flags to check
dnl $2: Action if the flags work
dnl $3: Action if the flags do not work
AC_DEFUN([_ORTE_SETUP_DEBUGGER_FLAGS_TRY_CFLAGS],[
    OPAL_VAR_SCOPE_PUSH([ORTE_SETUP_DEBUGGER_FLAGS_CFLAGS_save])

    ORTE_SETUP_DEBUGGER_FLAGS_CFLAGS_save=$CFLAGS
    AC_MSG_CHECKING([if $1 compiler flag works])
    CFLAGS="$CFLAGS $1"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[int i = 3;])],
        [ORTE_SETUP_DEBUGGER_FLAGS_HAPPY=yes],
        [ORTE_SETUP_DEBUGGER_FLAGS_HAPPY=no])
    AC_MSG_RESULT([$ORTE_SETUP_DEBUGGER_FLAGS_HAPPY])
    CFLAGS=$ORTE_SETUP_DEBUGGER_FLAGS_CFLAGS_save

    OPAL_VAR_SCOPE_POP

    AS_IF([test $ORTE_SETUP_DEBUGGER_FLAGS_HAPPY = yes],
          [$2], [$3])
])

AC_DEFUN([ORTE_SETUP_DEBUGGER_FLAGS],[
    #
    # Do a final process of the CFLAGS to make a WITHOUT_OPTFLAGS
    # version.  We need this so that we can guarantee to build the
    # debugger-sensitive files with -g and nothing else.
    #

    OPAL_STRIP_OPTFLAGS($CFLAGS)
    CFLAGS_WITHOUT_OPTFLAGS="$s_result"
    # Tweak the compiler flags passed to orterun for Sun Studio SPARC
    # https://svn.open-mpi.org/trac/ompi/ticket/1448
    if test "x$opal_cv_c_compiler_vendor" = "xsun" && test -n "`echo $host | $GREP sparc`"; then
        DEBUGGER_CFLAGS="-g -xO0"
    else
        # Tweak the compiler flags passed for intel
        # to stop its aggressive inlining of functions
        if test "x$opal_cv_c_compiler_vendor" = "xintel"; then
            DEBUGGER_CFLAGS="-g -O0"
        else
            DEBUGGER_CFLAGS="-g"
        fi
    fi
    AC_MSG_CHECKING([which of CFLAGS are ok for debugger modules])
    AC_MSG_RESULT([$CFLAGS_WITHOUT_OPTFLAGS])
    AC_MSG_CHECKING([for debugger extra CFLAGS])
    AC_MSG_RESULT([$DEBUGGER_CFLAGS])

    AC_SUBST(CFLAGS_WITHOUT_OPTFLAGS)
    AC_SUBST(DEBUGGER_CFLAGS)

    # Check for compiler specific flag to add in unwind information.
    # This is needed when attaching using MPIR to unwind back to the
    # user's main function. Certain optimisations can prevent GDB from
    # producing a stack when explicit unwind information is unavailable.
    # This is implied by -g, but we want to save space and don't need
    # full debug symbols.
    _ORTE_SETUP_DEBUGGER_FLAGS_TRY_CFLAGS([-fasynchronous-unwind-tables],
        [MPIR_UNWIND_CFLAGS="-fasynchronous-unwind-tables"],
        [_ORTE_SETUP_DEBUGGER_FLAGS_TRY_CFLAGS([-Meh_frame -Mframe],
            [MPIR_UNWIND_CFLAGS="-Meh_frame -Mframe"],
            [MPIR_UNWIND_CFLAGS=-g])
        ])

    AC_MSG_CHECKING([for final compiler unwind flags])
    AC_MSG_RESULT([$MPIR_UNWIND_CFLAGS])

    AC_SUBST(MPIR_UNWIND_CFLAGS)
])
