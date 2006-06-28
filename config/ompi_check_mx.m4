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

AC_DEFUN([_OMPI_CHECK_MX_REGISTER_CALLBACK],[
    #
    # Check if the MX library provide the mx_register_unexp_callback function.
    # With this function, OMPI can avoid having a double matching logic
    # (one on the MX library and one on OMPI) by registering our own matching
    # function.

    AC_MSG_CHECKING([for a MX version with mx_register_match_callback])
    AC_TRY_LINK([#include <myriexpress.h>],
             [mx_register_unexp_callback(0, 0, 0);],
             [mx_provide_match_callback="yes"],
             [mx_provide_match_callback="no"])
    AC_MSG_RESULT([$mx_provide_match_callback])
    AS_IF([test x"$mx_provide_match_callback" = "xyes"],
          [AC_DEFINE_UNQUOTED([OMPI_MCA_MX_HAVE_MATCH_CALLBACK], [1],
                              [MX define a match callback])
           $2],
          [$3])
    unset mx_provide_match_callback
])

AC_DEFUN([_OMPI_CHECK_MX_CONFIG],[
    #
    # See if we have MX_API. OpenMPI require a MX version bigger than
    # the first MX relase (0x300)
    #

    # restored at end of OMPI_CHECK_MX
    CPPFLAGS="$ompi_check_mx_$1_save_CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$ompi_check_mx_$1_save_LDFLAGS $$1_LDFLAGS"
    LIBS="$ompi_check_mx_$1_save_LIBS $$1_LIBS"

    AC_MSG_CHECKING([for MX version 1.0 or later])
    AC_TRY_COMPILE([#include <myriexpress.h>],
        [#if MX_API < 0x300
         #error "Version less than 0x300"
         #endif],
         [have_recent_api="yes"],
         [have_recent_api="no"])
    AC_MSG_RESULT([$have_recent_api])

    AC_MSG_CHECKING(for MX_API)
    AS_IF([test x"$have_recent_api" = "xyes"],
          [AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_API_VERSION], $mx_api_ver,
                [Version of the MX API to use])
           unset mx_api_ver have_mx_api_ver_msg found val msg
           $2],
          [$3])
    AC_MSG_RESULT([$mx_api_ver])
])dnl

# OMPI_CHECK_MX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if MX support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MX],[
    AC_ARG_WITH([mx],
        [AC_HELP_STRING([--with-mx(=DIR)],
             [Build MX (Myrinet Express) support, searching for libraries in DIR])])
    AC_ARG_WITH([mx-libdir],
        [AC_HELP_STRING([--with-mx-libdir=DIR],
             [Search for MX (Myrinet Express) libraries in DIR])])

    ompi_check_mx_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_mx_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_mx_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_mx" != "no"],
          [AS_IF([test ! -z "$with_mx" -a "$with_mx" != "yes"],
                 [ompi_check_mx_dir="$with_mx"])
           AS_IF([test ! -z "$with_mx_libdir" -a "$with_mx_libdir" != "yes"],
                 [ompi_check_mx_libdir="$with_mx_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [myriexpress.h],
                              [myriexpress],
                              [mx_finalize],
                              [],
                              [$ompi_check_mx_dir],
                              [$ompi_check_mx_libdir],
                              [ompi_check_mx_happy="yes"],
                              [ompi_check_mx_happy="no"])],
          [ompi_check_mx_happy="no"])

    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_CONFIG($1, [ompi_check_mx_happy="yes"],
                                 [ompi_check_mx_happy="no"])])
    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_REGISTER_CALLBACK($1, [ompi_check_mx_register="yes"],
                                 [ompi_check_mx_register="no"])])

    CPPFLAGS="$ompi_check_mx_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_mx_$1_save_LDFLAGS"
    LIBS="$ompi_check_mx_$1_save_LIBS"

    AS_IF([test "$ompi_check_mx_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([MX driver does not currently support progress threads.  Disabling BTL.])
           ompi_check_mx_happy="no"])

    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mx" -a "$with_mx" != "no"],
                 [AC_MSG_ERROR([MX support requested but not found.  Aborting])])
           $3])
])

