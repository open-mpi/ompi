# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Main PLPA m4 macro, to be invoked by the user
#
# Expects two paramters:
# 1. What to do upon success
# 2. What to do upon failure
#
AC_DEFUN([PLPA_INIT],[
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])
    AC_REQUIRE([AC_PROG_CC])

    # Check for syscall()
    AC_CHECK_FUNC([syscall], [happy=1], [happy=0])

    # Look for syscall.h
    if test "$happy" = 1; then
        AC_CHECK_HEADER([sys/syscall.h], [happy=1], [happy=0])
    fi

    # Look for unistd.h
    if test "$happy" = 1; then
        AC_CHECK_HEADER([unistd.h], [happy=1], [happy=0])
    fi

    # Check for __NR_sched_setaffinity
    if test "$happy" = 1; then
        AC_MSG_CHECKING([for __NR_sched_setaffinity])
        if test "$plpa_emulate" = "yes"; then
            AC_MSG_RESULT([emulated])
            AC_DEFINE([__NR_sched_setaffinity], [0], [Emulated value])
        else
            AC_TRY_COMPILE([#include <syscall.h>
#include <unistd.h>], [#ifndef __NR_sched_setaffinity
#error __NR_sched_setaffinity_not found!
#endif
int i = 1;],
                           [AC_MSG_RESULT([yes])
                            happy=1], 
                           [AC_MSG_RESULT([no])
                            happy=0])
        fi
    fi

    # Check for __NR_sched_getaffinity (probably overkill, but what
    # the heck?)
    if test "$happy" = 1; then
        AC_MSG_CHECKING([for __NR_sched_getaffinity])
        if test "$plpa_emulate" = "yes"; then
            AC_MSG_RESULT([emulated])
            AC_DEFINE([__NR_sched_getaffinity], [0], [Emulated value])
        else
            AC_TRY_COMPILE([#include <syscall.h>
#include <unistd.h>], [#ifndef __NR_sched_getaffinity
#error __NR_sched_getaffinity_not found!
#endif
int i = 1;],
                           [AC_MSG_RESULT([yes])
                            happy=1], 
                           [AC_MSG_RESULT([no])
                            happy=0])
        fi
    fi

    # If all was good, do the real init
    AS_IF([test "$happy" = "1"],
          [_PLPA_INIT($1, $2)],
          [$2])
    PLPA_DO_AM_CONDITIONALS

    # Cleanup
    unset happy
])dnl

#-----------------------------------------------------------------------

# Build PLPA as a standalone package
AC_DEFUN([PLPA_STANDALONE],[
    m4_define([plpa_config_prefix],[.])
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])
    plpa_mode=standalone
])dnl

#-----------------------------------------------------------------------

# Build PLPA as an included package
AC_DEFUN([PLPA_INCLUDED],[
    m4_define([plpa_config_prefix],[$1])
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])
    plpa_mode=included
    plpa_executables=no
])dnl

#-----------------------------------------------------------------------

# Set whether the fortran bindings will be built or not
AC_DEFUN([PLPA_FORTRAN],[
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])

    # Need [] around entire following line to escape m4 properly
    [plpa_tmp=`echo $1 | tr '[:upper:]' '[:lower:]'`]
    if test "$1" = "0" -o "$1" = "n"; then
        plpa_fortran=no
    elif test "$1" = "1" -o "$1" = "y"; then
        plpa_fortran=yes
    else
	AC_MSG_WARN([Did not understand PLPA_FORTRAN argument ($1) -- ignored])
    fi
])dnl

#-----------------------------------------------------------------------

# Set whether to build and install the executables or not
AC_DEFUN([PLPA_EXECUTABLES],[
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])

    # Need [] around entire following line to escape m4 properly
    [plpa_tmp=`echo $1 | tr '[:upper:]' '[:lower:]'`]
    if test "$1" = "0" -o "$1" = "n"; then
        plpa_executables=no
    elif test "$1" = "1" -o "$1" = "y"; then
        plpa_executables=yes
    else
	AC_MSG_WARN([Did not understand PLPA_EXECUTABLES argument ($1) -- ignored])
    fi

#
# need to check for plpa_mode = included and plpa_executables=yes
# and print a big AC_MSG_WARN if both are true

    if test "$plpa_mode" = "included" -a "$plpa_executables" = "yes" ; then
        AC_MSG_WARN([ Attempting to build executables in included mode ])
	AC_MSG_WARN([ This will fail - libplpa_included will be built ])
	AC_MSG_WARN([ Executables expect libplpa ])
    fi

])dnl

#-----------------------------------------------------------------------

# Specify the symbol prefix
AC_DEFUN([PLPA_SET_SYMBOL_PREFIX],[
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])
    plpa_symbol_prefix_value=$1
])dnl

#-----------------------------------------------------------------------

# Internals
AC_DEFUN([_PLPA_INTERNAL_SETUP],[

    AC_ARG_ENABLE([emulate],
                    AC_HELP_STRING([--enable-emulate],
                                   [Emulate __NR_sched_setaffinity and __NR_sched_getaffinity, to allow building on non-Linux systems (for testing)]))
    if test "$enable_emulate" = "yes"; then
        plpa_emulate=yes
    else
        plpa_emulate=no
    fi

    # Included mode, or standalone?
    AC_ARG_ENABLE([included-mode],
                    AC_HELP_STRING([--enable-included-mode],
                                   [Using --enable-included-mode puts the PLPA into "included" mode.  The default is --disable-included-mode, meaning that the PLPA is in "standalone" mode.]))
    if test "$enable_included_mode" = "yes"; then
        plpa_mode=included
    else
        plpa_mode=standalone
    fi

    # Fortran bindings, or no?
    AC_ARG_ENABLE([fortran],
                    AC_HELP_STRING([--disable-fortran],
                                   [Using --disable-fortran disables building the Fortran PLPA API bindings]))
    if test "$enable_fortran" = "yes" -o "$enable_fortran" = ""; then
        plpa_fortran=yes
    else
        plpa_fortran=no
    fi

    # Build and install the executables or no?
    AC_ARG_ENABLE([executables],
                    AC_HELP_STRING([--disable-executables],
                                   [Using --disable-executables disables building and installing the PLPA executables]))
    if test "$enable_executables" = "yes" -o "$enable_executables" = ""; then
        plpa_executables=yes
    else
        plpa_executables=no
    fi

    # Change the symbol prefix?
    AC_ARG_WITH([plpa-symbol-prefix],
                AC_HELP_STRING([--with-plpa-symbol-prefix=STRING],
                               [STRING can be any valid C symbol name.  It will be prefixed to all public PLPA symbols.  Default: "plpa_"]))
    if test "$with_plpa_symbol_prefix" = ""; then
        plpa_symbol_prefix_value=plpa_
    else
        plpa_symbol_prefix_value=$with_plpa_symbol_prefix
    fi
])dnl

#-----------------------------------------------------------------------

# Internals for PLPA_INIT
AC_DEFUN([_PLPA_INIT],[
    AC_REQUIRE([_PLPA_INTERNAL_SETUP])

    # Are we building as standalone or included?
    AC_MSG_CHECKING([for PLPA building mode])
    AC_MSG_RESULT([$plpa_mode])

    # We need to set a path for header, etc files depending on whether
    # we're standalone or included. this is taken care of by PLPA_INCLUDED.
    # Move AC_CONFIG_HEADER and AC_CONFIG_FILES to here, using the
    # path prefix from PLPA_INCLUDED

    AC_MSG_CHECKING([for PLPA config prefix])
    AC_MSG_RESULT(plpa_config_prefix)

    AM_CONFIG_HEADER(plpa_config_prefix[/src/libplpa/plpa.h])
    AC_CONFIG_FILES(
        plpa_config_prefix[/Makefile]
        plpa_config_prefix[/config/Makefile]
        plpa_config_prefix[/src/Makefile]
        plpa_config_prefix[/src/libplpa/Makefile]
        plpa_config_prefix[/src/plpa-info/Makefile]
        plpa_config_prefix[/src/plpa-taskset/Makefile]
    )

    # What prefix are we using?
    AC_MSG_CHECKING([for PLPA symbol prefix])
    AC_DEFINE_UNQUOTED(PLPA_SYM_PREFIX, [$plpa_symbol_prefix_value],
                       [The PLPA symbol prefix])
    # Ensure to [] escape the whole next line so that we can get the
    # proper tr tokens
    [plpa_symbol_prefix_value_caps="`echo $plpa_symbol_prefix_value | tr '[:lower:]' '[:upper:]'`"]
    AC_DEFINE_UNQUOTED(PLPA_SYM_PREFIX_CAPS, [$plpa_symbol_prefix_value_caps],
                       [The PLPA symbol prefix in all caps])
    AC_MSG_RESULT([$plpa_symbol_prefix_value])

    # Check for fortran
    AC_MSG_CHECKING([whether to build PLPA Fortran API])
    AC_MSG_RESULT([$plpa_fortran])

    # Check whether to build the exectuables or not
    AC_MSG_CHECKING([whether to build PLPA executables])
    AC_MSG_RESULT([$plpa_executables])

    # If we're building executables, we need some things for plpa-taskset
    if test "$plpa_executables" = "yes"; then
        AC_C_INLINE
    fi
    # Always need these for make dist (both included and standalone)
    AM_PROG_LEX
    AC_PROG_YACC

    # Success
    $1
])dnl


#-----------------------------------------------------------------------

# This must be a standalone routine so that it can be called both by
# PLPA_INIT and an external caller (if PLPA_INIT is not invoked).
AC_DEFUN([PLPA_DO_AM_CONDITIONALS],[
    if test "$plpa_did_am_conditionals" != "yes"; then
        AM_CONDITIONAL([PLPA_BUILD_STANDALONE], [test "$plpa_mode" = "standalone"])
        AM_CONDITIONAL(PLPA_BUILD_FORTRAN, [test "$plpa_fortran" = "yes"])
        AM_CONDITIONAL(PLPA_BUILD_EXECUTABLES, [test "$plpa_executables" = "yes"])
    fi
    plpa_did_am_conditionals=yes
])dnl
